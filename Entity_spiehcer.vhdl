use subtype_package_all.all;

entity Speicher is  //Entity für Speicher
   port(
        clk : in boolean;   //Clock
        addr: in mem_data;   // addr  datatyp ist von Subtype_package_all deklariert
        data_in: in mem_data;   //Data.input
        data_out: out mem_data);  //Data.output
end Speicher;

architecture function_S of Speicher is        
begin
    process( clk , addr , data_in)    //empfindliche Signal
    variable Mem: mem_type;           // typ mem_type ist von subtype_package_all deklariert
    begin
       if clk then                    // wie Speicher funktioniert.
          Mem(addr) := data_in;     
       end if; 
    data_out <= Mem(addr);
    end process;
end function_S;
