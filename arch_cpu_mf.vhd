LIBRARY IEEE; 
USE IEEE.std_logic_1164.ALL;
Use work.def_package_all.all;                   -- unsere Datentypen, etc.
Use work.mem_package_all.all;                   -- unser Speicherinhalt / abzuarbeitendes Programm
ARCHITECTURE behav OF CPU IS
  BEGIN
    Process 
      -- Speicher
      variable Memory: mem_type := init_memory; -- Speicher mit init_memory initialisieren
      variable Reg   :reg_type := (0 => 0, 1=> 0, others => 0);		-- 2.1.2.2 bisher nicht verwendet (wird für die Operationen benötigt)
      variable Instr :data_type;		-- Aus dem Speicher geholte Anweisung
      variable OP    :opcode_type;		-- aus der Instr errechneter OPCode (ableich mit unserem def_pack)
      variable x,y,z :reg_addr_type;		-- Registeradressen zur Verarbeitung unserer Anweisungen
      variable PC    :addr_type:=0;		-- 2.1.3.1; 2.1.3.2 unser Prozesscounter
			variable Zero, Carry, Negative, Overflow: Boolean := FALSE;
      
      begin
        Instr := Memory(PC); OP:= Instr / (2**reg_addr_width)**3; -- Anweisung lesen
        x:=(Instr / (2**reg_addr_width)**2)mod 2**reg_addr_width; -- Anweisung zerlegen
        y:= (Instr / 2** reg_addr_width) mod 2** reg_addr_width;  -- Anweisung zerlegen
        z:= Instr mod 2** reg_addr_width;                         -- Anweisung zerlegen

        PC:= INC(PC);
        
        case OP is                                                -- Anweisungen differenzieren und ausführen

          when code_nop   => null;   			-- keine Operation 	(3.3.1.1)    
          when code_stop  => wait;   			-- stop Simulation 	(3.3.1.2)
					
					-- die OPCode Operationen hier einfügen (s.h. Vorlesung ?? Seite 49ff. - Statements for Arithmetic and Logic Ops)

					when code_not		=> Reg(x):= "NOT" Reg(y);		-- Verneinung 			(3.3.1.7)
					when code_and		=> Reg(x):=y "and" Reg(z);	-- UND-Operation		(3.3.1.8)
					when code_or		=> Reg(x):=Reg(y) "or" Reg(z);		-- OR-Operation			(3.3.1.9)
					when code_xor		=> Reg(x):=Reg(y) "xor" Reg(z);	-- xor							(3.3.1.10)
					when code:rea		=> 

          when others =>             -- ungültig oder bisher nicht implementiert
            assert FALSE
            report "ungültig"
            severity error;
        end case;
      wait;                                     --damit der Prozessor nicht den Speicher im endlos modus durcharbeitet
    end process;
END behav;
