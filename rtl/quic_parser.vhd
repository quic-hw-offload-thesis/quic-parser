-- ------------------------------------------------------------------------------
-- Master's thesis: Hardware/software co-design for the new QUIC network protocol
-- ------------------------------------------------------------------------------
-- QUIC Packet Parser
--
-- File: quic_parser.vhd (vhdl)
-- By: Lowie Deferme (UHasselt/KULeuven - FIIW)
-- On: 09 June 2022
-- ------------------------------------------------------------------------------

library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

library work;
    use work.quic_offload_pkg.all;

entity quic_parser is
    generic (
        G_MD_IN_WIDTH : integer := 32;
        G_MD_OUT_WIDTH : integer := 32
    );
    port (
        -- Control ports
        clk : in std_logic;
        reset : in std_logic;
        -- Data
        d_in : in std_logic_vector(C_DATA_WIDTH-1 downto 0);
        d_out : out std_logic_vector(C_DATA_WIDTH-1 downto 0);
        -- Metadata
        md_in : in std_logic_vector(G_MD_IN_WIDTH-1 downto 0);
        md_out : out std_logic_vector(G_MD_OUT_WIDTH-1 downto 0)
    );
end quic_parser;

architecture rtl of quic_parser is

    -- (De)localised IO
    signal clk_i : std_logic;
    signal reset_i : std_logic;
    signal d_in_i : std_logic_vector(C_DATA_WIDTH-1 downto 0);
    signal d_out_i : std_logic_vector(C_DATA_WIDTH-1 downto 0);
    signal md_in_i : std_logic_vector(G_MD_IN_WIDTH-1 downto 0);
    signal md_out_i : std_logic_vector(G_MD_OUT_WIDTH-1 downto 0);

    -- Fsm states
    type T_fsm_state IS (R, F, PN, PL, M);  -- define states (Ready, Flags, Packet Number, Payload, Mac)
    signal fsm_state : T_fsm_state; -- fsm state signal
    signal fsm_state_next : T_fsm_state; -- next fsm state signal

    -- Control signals
    signal quic_mask : std_logic_vector(3 downto 0);
    signal dcid_mask : std_logic_vector(5 downto 0);
    signal packet_number_len : std_logic_vector(1 downto 0);
    signal packet_number_fixed_mask : std_logic_vector(3 downto 0); -- Four bit mask for the packet number, not shifted over multiple words
    signal dcid_end : std_logic;
    signal dcid_start : std_logic;
    signal packet_end : std_logic;
    signal strobe : std_logic_vector(3 downto 0);

    -- Masks
    signal flags_mask : std_logic_vector(3 downto 0);
    signal packet_number_mask : std_logic_vector(7 downto 0);
    signal payload_mask : std_logic_vector(3 downto 0);
    signal mac_mask : std_logic_vector(19 downto 0);

    -- Registers
    constant C_NUMOF_REGS : integer := 5; -- Should not be changed
    type T_data_regs is array (C_NUMOF_REGS downto 0) of std_logic_vector(C_DATA_WIDTH-1 downto 0);
    signal data_regs : T_data_regs;
    signal metadata_reg_0 : std_logic_vector(G_MD_IN_WIDTH-1 + 0 downto 0); -- No extra metadata
    signal metadata_reg_1 : std_logic_vector(G_MD_IN_WIDTH-1 + 4 downto 0); -- Mac metadata per byte
    signal metadata_reg_2 : std_logic_vector(G_MD_IN_WIDTH-1 + 4 downto 0); -- Mac metadata per byte
    signal metadata_reg_3 : std_logic_vector(G_MD_IN_WIDTH-1 + 4 downto 0); -- Mac metadata per byte
    signal metadata_reg_4 : std_logic_vector(G_MD_IN_WIDTH-1 + 12 downto 0); -- Mac, packet number and flags metadata per byte
    signal metadata_reg_5 : std_logic_vector(G_MD_IN_WIDTH-1 + 16 downto 0); -- Mac, packet number, flags and payload metadata per byte
 
begin

    -----------------------------------------------------------------------------
    -- (De)localising IO
    -----------------------------------------------------------------------------
    clk_i <= clk;
    reset_i <= reset;
    d_in_i <= d_in;
    d_out <= d_out_i;
    md_in_i <= md_in;
    md_out <= md_out_i;

    -----------------------------------------------------------------------------
    -- Define masks
    -----------------------------------------------------------------------------

    -- Flags mask
    G_FLAGS: for i in 0 to 3 generate
        flags_mask(i) <= '1' when (dcid_mask(i) = '1' and dcid_mask(i+1) = '0') else '0';
    end generate G_FLAGS;

    -- Packet Number mask:
    -- -------------------
    -- Byte with index 0 arrived last                             |  \      
    --          |\                                                |   \
    --          | \                                               |    \
    --  1000 ---|00|                               |---MMMM0000---|10000|
    --  1100 ---|01|---packet_number_fixed_mask---<|---0MMMM000---|11000|
    --  1110 ---|10|                               |---00MMMM00---|11100|--- packet_number_mask
    --  1111 ---|11|                               |---000MMMM0---|11110|
    --          | /            00000000---------------------------|other|
    --          |/|                                               |    /
    --            |                                               |   /
    --            |                                               |  /|
    --            |                                                   |
    --    packet_number_len                                      dcid_mask(5 downto 1)
    -- 
    with packet_number_len select packet_number_fixed_mask <= 
        "1000" when "00",
        "1100" when "01",
        "1110" when "10",
        "1111" when "11",
        "0000" when others;
    packet_number_mask <= 
        (packet_number_fixed_mask & "0000") when (dcid_mask(5 downto 1) = "10000") else
        ("0" & packet_number_fixed_mask & "000") when (dcid_mask(5 downto 1) = "11000") else
        ("00" & packet_number_fixed_mask & "00") when (dcid_mask(5 downto 1) = "11100") else
        ("000" & packet_number_fixed_mask & "0") when (dcid_mask(5 downto 1) = "11110") else
        "00000000";
    
    -- Mac mask
    mac_mask <= (not strobe & x"fff" & strobe) when (packet_end = '1') else (others => '0');
    
    -----------------------------------------------------------------------------
    -- Control signals
    -----------------------------------------------------------------------------

    quic_mask(3) <= '1' when (metadata_reg_4(C_MO_PROTO_BYTE_3+C_MS_PROTO-1 downto C_MO_PROTO_BYTE_3) = C_PROTO_1RTT_QUIC) else '0';
    quic_mask(2) <= '1' when (metadata_reg_4(C_MO_PROTO_BYTE_2+C_MS_PROTO-1 downto C_MO_PROTO_BYTE_2) = C_PROTO_1RTT_QUIC) else '0';
    quic_mask(1) <= '1' when (metadata_reg_4(C_MO_PROTO_BYTE_1+C_MS_PROTO-1 downto C_MO_PROTO_BYTE_1) = C_PROTO_1RTT_QUIC) else '0';
    quic_mask(0) <= '1' when (metadata_reg_4(C_MO_PROTO_BYTE_0+C_MS_PROTO-1 downto C_MO_PROTO_BYTE_0) = C_PROTO_1RTT_QUIC) else '0';
    
    -- Dcid metadata: last arrived of reg 4 & reg 3 & first arrived of reg 2
    dcid_mask(5) <= metadata_reg_4(C_MO_DCID_BYTE_0);
    dcid_mask(4) <= metadata_reg_3(C_MO_DCID_BYTE_3);
    dcid_mask(3) <= metadata_reg_3(C_MO_DCID_BYTE_2);
    dcid_mask(2) <= metadata_reg_3(C_MO_DCID_BYTE_1);
    dcid_mask(1) <= metadata_reg_3(C_MO_DCID_BYTE_0);
    dcid_mask(0) <= metadata_reg_2(C_MO_DCID_BYTE_3);
    
    -- Dcid ends somewhere in between if oldest byte is still dcid and newest isn't anymore
    dcid_end <= '1' when (dcid_mask(5) = '1' and dcid_mask(1) = '0') else '0';

    -- Dcid starts somewhere in between if oldest byte is not dcid and newest is
    dcid_start <= '1' when (dcid_mask(4) = '0' and dcid_mask(0) = '1') else '0'; -- Fixme: is this signal necessary?

    -- Packet ends when data last is '1' in newest data
    packet_end <= metadata_reg_0(C_MO_DATA_LAST);
    strobe <= metadata_reg_0(C_MO_STROBE+C_MS_STROBE-1 downto C_MO_STROBE);

    -- Note: 1-RTT Packet
    -- ------------------
    --     Header Form (1) = 0,
    --     Fixed Bit (1) = 1,
    --     Spin Bit (1),
    --     Reserved Bits (2),                   # Protected
    --     Key Phase (1),                       # Protected
    --     Packet Number Length (2),            # Protected
    --     Destination Connection ID (0..160),
    --     Packet Number (8..32),               # Protected
    --     Packet Payload (8..),                # Protected
    P_PNL : process(clk_i, reset_i)
    begin
        if reset_i = '1' then
            packet_number_len <= "00";
        elsif rising_edge(clk_i) then
            case flags_mask is
                when "0001" => packet_number_len <= data_regs(3)(1 downto 0);
                when "0010" => packet_number_len <= data_regs(3)(9 downto 8);
                when "0100" => packet_number_len <= data_regs(3)(17 downto 16);
                when "1000" => packet_number_len <= data_regs(3)(25 downto 24);
                when others => packet_number_len <= packet_number_len;
            end case;
        end if;
    end process ; -- P_PNL

    -----------------------------------------------------------------------------
    -- Registers
    -----------------------------------------------------------------------------

    -- Metadata
    metadata_reg_0 <= md_in_i;
    P_MD_REG : process(clk_i, reset_i)
    begin
        if reset_i = '1' then
            metadata_reg_1 <= (others => '0');
            metadata_reg_2 <= (others => '0');
            metadata_reg_3 <= (others => '0');
            metadata_reg_4 <= (others => '0');
            metadata_reg_5 <= (others => '0'); 
        elsif rising_edge(clk_i) then
            metadata_reg_1 <= x"0" & metadata_reg_0; -- Todo: concat correct masker signals
            metadata_reg_2 <= metadata_reg_1;
            metadata_reg_3 <= metadata_reg_2;
            metadata_reg_4 <= x"0" & flags_mask & metadata_reg_3;
            metadata_reg_5 <= x"0" & metadata_reg_4;
        end if;
    end process ; -- P_MD_REG
    md_out_i <= metadata_reg_5;

    -- Data
    data_regs(0) <= d_in_i;
    G_D_REG: for i in 1 to C_NUMOF_REGS generate
        P_D_REG : process(clk_i, reset_i)
        begin
            if reset_i = '1' then
                data_regs(i) <= (others => '0');
            elsif rising_edge(clk_i) then
                data_regs(i) <= data_regs(i-1);
            end if;
        end process ; -- P_D_REG
    end generate G_D_REG;
    d_out_i <= data_regs(C_NUMOF_REGS);
    
    -----------------------------------------------------------------------------
    -- Finite State Machine
    -----------------------------------------------------------------------------
    P_NSF : process(fsm_state) -- Fixme: write better next state function
    begin
        case fsm_state is
            when R => fsm_state_next <= F;-- when quic_mask /= b"0000" else R;
            when F => fsm_state_next <= PN;
            when PN => fsm_state_next <= PL; -- Todo: fsm next state function
            when PL => fsm_state_next <= M; -- Todo: fsm next state function
            when M => fsm_state_next <= R; -- Todo: fsm next state function
            when others => fsm_state_next <= R;
        end case;
    end process ; -- P_NSF
    
    P_FSM : process(clk_i, reset_i)
    begin
        if reset_i = '1' then
            fsm_state <= R;
        elsif rising_edge(clk_i) then
            fsm_state <= fsm_state_next;
        end if;
    end process ; -- P_FSM
    
end architecture rtl;