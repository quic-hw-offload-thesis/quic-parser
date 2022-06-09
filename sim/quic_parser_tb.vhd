-- ------------------------------------------------------------------------------
-- Master's thesis: Hardware/software co-design for the new QUIC network protocol
-- ------------------------------------------------------------------------------
-- QUIC Packet Parser
--
-- File: quic_parser_tb.vhd (vhdl)
-- By: Lowie Deferme (UHasselt/KULeuven - FIIW)
-- On: 09 June 2022
-- ------------------------------------------------------------------------------


library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    use ieee.std_logic_textio.all;
    use std.textio.all;

library work;
    use work.quic_offload_pkg.all;

entity quic_parser_tb is
end entity quic_parser_tb;

architecture behavioural of quic_parser_tb is

    -- Define clock period
    constant clock_period : time := 10 ns;

    -- Constant to hold metadata width
    constant META_DATA_IN_WIDTH : integer := 22;
    constant META_DATA_OUT_WIDTH : integer := META_DATA_IN_WIDTH + 16;

    -- Define (meta)data input file
    constant FNAME_DATA : string := "C:/Users/ldefe/Documents/IIW/MP/project/quic-hw-offload/quic_parser/res/data.dat";
    file f_data : text;

    -- Memory to hold (meta)data
    type T_data_memory is array(0 to 16384-1) of STD_LOGIC_VECTOR(C_DATA_WIDTH-1 downto 0);
    type T_metadata_memory is array(0 to 16384-1) of STD_LOGIC_VECTOR(META_DATA_IN_WIDTH-1 downto 0);
    signal data_mem : T_data_memory;
    signal metadata_mem : T_metadata_memory;
    
    -- Define clock and reset
    signal reset : std_logic;
    signal clock : std_logic;

    -- Input signals
    signal dut_d_in : std_logic_vector(C_DATA_WIDTH-1 downto 0);
    signal dut_md_in : std_logic_vector(META_DATA_IN_WIDTH-1 downto 0);

    -- Output signals
    signal dut_d_out : std_logic_vector(C_DATA_WIDTH-1 downto 0);
    signal dut_md_out : std_logic_vector(META_DATA_OUT_WIDTH-1 downto 0);
    
begin

    -----------------------------------------------------------------------------
    -- DUT
    -----------------------------------------------------------------------------
    quic_parser_dut : component quic_parser
        generic map (
            G_MD_IN_WIDTH => META_DATA_IN_WIDTH,
            G_MD_OUT_WIDTH => META_DATA_OUT_WIDTH
        )
        port map (
            clk => clock,
            reset => reset,
            d_in => dut_d_in,
            d_out => dut_d_out,
            md_in => dut_md_in,
            md_out => dut_md_out
        );

    -----------------------------------------------------------------------------
    -- Clock process
    -----------------------------------------------------------------------------

    P_CLK : process
    begin
        clock <= '1';
        wait for clock_period/2;
        clock <= '0';
        wait for clock_period/2;
    end process ; -- P_CLK

    -----------------------------------------------------------------------------
    -- Reset process
    -----------------------------------------------------------------------------

    P_RST : process
    begin
        reset <= '1';
        wait for clock_period*5;
        reset <= '0';
        wait;
    end process ; -- P_RST

    -----------------------------------------------------------------------------
    -- (Meta)data driver
    -----------------------------------------------------------------------------

    P_DATA : process(clock, reset)
        variable data_pointer : integer; -- Pointer to current (meta)data
        variable v_pointer : integer;
        variable v_line : line;
        variable v_temp : STD_LOGIC_VECTOR(C_DATA_WIDTH+META_DATA_IN_WIDTH-1 downto 0);
    begin
        if reset = '1' then
            dut_d_in <= (others => '0');
            dut_md_in <= (others => '0');

            data_pointer := 0;
            v_pointer := 0;

            file_open(f_data, FNAME_DATA, read_mode);

            while not endfile(f_data) loop
                readline(f_data, v_line);
                read(v_line, v_temp);
                data_mem(v_pointer) <= v_temp(C_DATA_WIDTH+META_DATA_IN_WIDTH-1 downto META_DATA_IN_WIDTH);
                metadata_mem(v_pointer) <= v_temp(META_DATA_IN_WIDTH-1 downto 0);
                v_pointer := v_pointer + 1;
            end loop;
            
            file_close(f_data);
        elsif rising_edge(clock) then
            dut_d_in <= data_mem(data_pointer);
            dut_md_in <= metadata_mem(data_pointer);
            data_pointer := data_pointer + 1;
        end if;
    end process ; -- P_DATA

end architecture behavioural;
