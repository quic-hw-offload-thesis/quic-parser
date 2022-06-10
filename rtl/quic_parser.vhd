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

    -- Control/internal signals
    signal dcid_mask : std_logic_vector(5 downto 0);                -- Dcid mask of R5B0 & R4B3..0 & R2B3
    signal packet_number_len : std_logic_vector(1 downto 0);        -- Raw packet number len field (= actual len-1)
    signal packet_number_fixed_mask : std_logic_vector(3 downto 0); -- Four bit mask for the packet number, not shifted over multiple words
    signal dcid_end : std_logic;                    -- Pulse whenever dcid ends at reg 4  
    signal dcid_end_delay : std_logic;              -- Pulse whenever dcid ends at reg 5
    signal packet_end_0 : std_logic;                -- Pulse whenever packet ends at reg 0
    signal packet_end_5 : std_logic;                -- Pulse whenever packet ends at reg 5
    signal strobe_0 : std_logic_vector(3 downto 0); -- Strobe of reg 0
    signal is_mac_mask : std_logic;                 -- High whenever metadata is mac mask (thus should be shifted through without change)
    signal packet_number_mask_delay : std_logic_vector(3 downto 0); -- Holds packet number mask for reg 5
    signal is_payload : std_logic;                  -- High whenever payload in reg 5

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
    signal metadata_reg_4 : std_logic_vector(G_MD_IN_WIDTH-1 + 4 downto 0); -- Mac metadata per byte
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
    mac_mask <= (not strobe_0 & x"fff" & strobe_0) when (packet_end_0 = '1') else (others => '0');

    -- Payload mask
    payload_mask <= 
        "0000" when is_payload = '0' else
        strobe_0 when packet_end_0 = '1' else
        not packet_number_mask_delay(3 downto 0) when dcid_end_delay = '1' else
        "1111";
        
    -----------------------------------------------------------------------------
    -- Control/internal signals
    -----------------------------------------------------------------------------

    -- Dcid metadata: last arrived of reg 4 & reg 3 & first arrived of reg 2
    dcid_mask(5) <= metadata_reg_5(C_MO_DCID_BYTE_0);
    dcid_mask(4) <= metadata_reg_4(C_MO_DCID_BYTE_3);
    dcid_mask(3) <= metadata_reg_4(C_MO_DCID_BYTE_2);
    dcid_mask(2) <= metadata_reg_4(C_MO_DCID_BYTE_1);
    dcid_mask(1) <= metadata_reg_4(C_MO_DCID_BYTE_0);
    dcid_mask(0) <= metadata_reg_3(C_MO_DCID_BYTE_3);
    
    -- Dcid ends somewhere in between if oldest byte is still dcid and newest isn't anymore
    dcid_end <= '1' when (dcid_mask(5) = '1' and dcid_mask(1) = '0') else '0';
    P_DCID_END : process(clk_i, reset_i)
    begin
        if reset_i = '1' then
            dcid_end_delay <= '0';
        elsif rising_edge(clk_i) then
            dcid_end_delay <= dcid_end;
        end if;
    end process ; -- P_DCID_END

    -- Packet ends when data last is '1' in metadata
    packet_end_0 <= metadata_reg_0(C_MO_DATA_LAST);
    strobe_0 <= metadata_reg_0(C_MO_STROBE+C_MS_STROBE-1 downto C_MO_STROBE);
    packet_end_5 <= metadata_reg_5(C_MO_DATA_LAST);

    -- Mac starts when data last becomes '1' at reg 0 and ends when data last becomes '1' at reg 5
    P_IS_MAC : process(clk_i, reset_i)
    begin
        if reset_i = '1' then
            is_mac_mask <= '0';
        elsif rising_edge(clk_i) then
            if packet_end_0 = '1' then
                is_mac_mask <= '1';
            elsif packet_end_5 = '1' then
                is_mac_mask <= '0';
            else
                is_mac_mask <= is_mac_mask;
            end if;
        end if;
    end process ; -- P_IS_MAC

    -- Only 4 lsb of packet number mask are needed for packet number mask delay since the rest is already written and shifted out
    P_PND : process(clk_i, reset_i)
    begin
        if reset_i = '1' then
            packet_number_mask_delay <= (others => '0');
        elsif rising_edge(clk_i) then
            packet_number_mask_delay <= packet_number_mask(3 downto 0);
        end if;
    end process ; -- P_PND

    -- Payload starts when packet number ends at reg 5 and stops when data valid at reg 0
    P_IPL : process(clk_i, reset_i)
    begin
        if reset_i = '1' then
            is_payload <= '0';
        elsif rising_edge(clk_i) then
            if dcid_end_delay = '1' then
                is_payload <= '1';
            elsif dcid_end = '1' and packet_number_mask(3 downto 0) /= "0000" then
                is_payload <= '1';
            elsif packet_end_0 = '1' then
                is_payload <= '0';
            else
                is_payload <= is_payload;
            end if;
        end if;
    end process ; -- P_IPL

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
                when "0001" => packet_number_len <= data_regs(4)(1 downto 0);
                when "0010" => packet_number_len <= data_regs(4)(9 downto 8);
                when "0100" => packet_number_len <= data_regs(4)(17 downto 16);
                when "1000" => packet_number_len <= data_regs(4)(25 downto 24);
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
            -- Shift metadata in
            metadata_reg_1(G_MD_IN_WIDTH-1 downto 0) <= metadata_reg_0(G_MD_IN_WIDTH-1 downto 0);
            metadata_reg_2(G_MD_IN_WIDTH-1 downto 0) <= metadata_reg_1(G_MD_IN_WIDTH-1 downto 0);
            metadata_reg_3(G_MD_IN_WIDTH-1 downto 0) <= metadata_reg_2(G_MD_IN_WIDTH-1 downto 0);
            metadata_reg_4(G_MD_IN_WIDTH-1 downto 0) <= metadata_reg_3(G_MD_IN_WIDTH-1 downto 0);
            metadata_reg_5(G_MD_IN_WIDTH-1 downto 0) <= metadata_reg_4(G_MD_IN_WIDTH-1 downto 0);

            -- Add Mac mask
            if packet_end_0 = '1' then -- Set if packet ends at reg 0
                metadata_reg_1(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH) <= mac_mask(3 downto 0);
                metadata_reg_2(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH) <= mac_mask(7 downto 4);
                metadata_reg_3(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH) <= mac_mask(11 downto 8);
                metadata_reg_4(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH) <= mac_mask(15 downto 12);
                metadata_reg_5(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH) <= mac_mask(19 downto 16);
            elsif is_mac_mask = '1' then -- Shift if outgoing data is mac
                metadata_reg_1(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH) <= (others => '0');
                metadata_reg_2(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH) <= metadata_reg_1(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH);
                metadata_reg_3(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH) <= metadata_reg_2(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH);
                metadata_reg_4(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH) <= metadata_reg_3(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH);
                metadata_reg_5(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH) <= metadata_reg_4(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH);
            else -- Reset otherwise
                metadata_reg_1(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH) <= (others => '0');
                metadata_reg_2(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH) <= (others => '0');
                metadata_reg_3(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH) <= (others => '0');
                metadata_reg_4(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH) <= (others => '0');
                metadata_reg_5(G_MD_IN_WIDTH+4-1 downto G_MD_IN_WIDTH) <= (others => '0');
            end if;

            -- Add Packet Number mask
            if dcid_end = '1' then -- Set reg 5 metadata when dcid ends at reg 4
                metadata_reg_5(G_MD_IN_WIDTH+8-1 downto G_MD_IN_WIDTH+4) <= packet_number_mask(7 downto 4);
            elsif dcid_end_delay = '1' then -- Shift if outgoing data is packet number
                metadata_reg_5(G_MD_IN_WIDTH+8-1 downto G_MD_IN_WIDTH+4) <= packet_number_mask_delay(3 downto 0);
            else -- Reset otherwise
                metadata_reg_5(G_MD_IN_WIDTH+8-1 downto G_MD_IN_WIDTH+4) <= (others => '0');
            end if;

            -- Add flag byte mask
            metadata_reg_5(G_MD_IN_WIDTH+12-1 downto G_MD_IN_WIDTH+8) <= flags_mask;

            -- Add Payload mask
            metadata_reg_5(G_MD_IN_WIDTH+16-1 downto G_MD_IN_WIDTH+12) <= payload_mask;

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
    
end architecture rtl;
