LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
Use work.def_package_all.all;                   -- unsere Datentypen, etc.
Use work.mem_package_all.all;                   -- unser Speicherinhalt / abzuarbeitendes Programm
ARCHITECTURE behav OF CPU IS
  BEGIN
    Process 
      -- Speicher
      variable Memory: mem_type := init_memory; -- Speicher mit init_memory initialisieren
      variable Reg   :reg_type;
      variable Instr :data_type;
      variable OP    :opcode_type;
      variable x,y,z :reg_addr_type;
      variable PC    :addr_type:=0;
      
      begin
        Instr := Memory(PC); OP:= Instr / (2**reg_addr_width)**3; -- Anweisung lesen
        x:=(Instr / (2**reg_addr_width)**2)mod 2**reg_addr_width; -- Anweisung zerlegen
        y:= (Instr / 2** reg_addr_width) mod 2** reg_addr_width;  -- Anweisung zerlegen
        z:= Instr mod 2** reg_addr_width;                         -- Anweisung zerlegen
        if PC=address_width - 1 
          then PC :=0;
          else PC:=PC+1;
        end if;
        
        case OP is                                                -- Anweisungen differenzieren und ausführen
          when code_nop   => null;   -- keine Operation (3.3.1.1)    die OPCode Operationen hier einfügen
          when code_stop  => wait;   -- stop Simulation (3.3.1.2)
          when others =>             -- ungültig oder bisher nicht implementiert
            assert FALSE
            report "ungültig"
            severity error;
        end case;
      wait;                                     --damit der Prozessor nicht den Speicher im endlos modus durcharbeitet
    end process;
END behav;
