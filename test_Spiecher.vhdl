entity test_Speicher is
end test_Speicher;

architecture TB of test_Speicher is
   component Speicher is
      port(
         clk : in boolean;
         addr: in integer range 4095 downto 0;
        data_in: in integer range 4095 downto 0;
        data_out: out integer range 4095 downto 0);
   end component;
signal clk: boolean;
signal addr, data_in , data_out:
           integer range 4095 downto 0;
begin
UUT:Speicher port map( clk, addr, data_in, data_out);
   clk <= not clk after 9 ns;
   data_in <= 42 after 9 ns, 0 after 21 ns;
   addr <= 1 after 9ns, 0 after 21 ns, 1 after 30 ns;
end TB;
