LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;


package def_package_all is
      
  --Hier alle Konstanten definieren
  constant data_width       :positive :=12;   --1.1.1
  constant address_width    :positive :=12;   --1.1.2
  constant opcode_width     :positive :=6;    --3.1.1
  
  constant reg_addr_width   :positive :=2;    --2.1.2
  
  
  --Hier alle Datentypen, Subtyps definieren
  subtype addr_type IS					-- unsers Adressen zum ansprechen des Speichers, z.B. durch den PC
    natural range 0 to 2**address_width-1;
  subtype opcode_type IS				-- für unsere OPCode Deklarationen
    natural range 0 to 2**opcode_width-1;
  subtype reg_addr_type is				-- zum Ansprechen unserer Register
    natural range 0 to 2**reg_addr_width-1;  
  subtype data_type is 					-- Typ, wie unsere Anweisungen, etc. im Speicher abgelegt werden
    natural range 0 to 2**data_width-1;  
  type mem_type is 					-- unser "Speicher" (Array)
    array(addr_type) of data_type;  			
  type reg_type is array(reg_addr_type) of data_type;	-- 2.1.2.1 unsere "Register" (Array)
  
  --Hier alle OPCodes definieren
  constant code_nop   : opcode_type:=0;       --3.3.1.1
  constant code_stop  : opcode_type:=1;       --3.3.1.2
  constant code_add   : opcode_type:=2;       --3.3.1.3
      
      
end def_package_all;
