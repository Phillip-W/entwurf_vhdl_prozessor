LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE work.def_package_all.ALL; -- unsere Datentypen, etc.
USE work.mem_package_all.ALL; -- unser Speicherinhalt / abzuarbeitendes Programm
USE std.textio.ALL; -- für unser IO
ARCHITECTURE behav OF CPU IS
BEGIN
	PROCESS
	FILE TraceFile: Text IS OUT "Trace";
	FILE DumpFile: Text IS OUT "Dump";
	VARIABLE l : line;
	VARIABLE Memory : mem_type := init_memory; -- Speicher mit init_memory initialisieren
	VARIABLE Reg : reg_type := (0 => 0, 1 => 0, OTHERS => 0); 
	VARIABLE Instr : data_type; -- Aus dem Speicher geholte Anweisung
	VARIABLE OP : opcode_type; -- aus der Instr errechneter OPCode (ableich mit unserem def_pack)
	VARIABLE a, x, y, z : reg_addr_type; -- Registeradressen zur Verarbeitung unserer Anweisungen
	VARIABLE PC : addr_type := 0; -- 2.1.3.1; 2.1.3.2 unser Prozesscounter
	VARIABLE Zero, Carry, Negative, Overflow : BOOLEAN := FALSE;
	VARIABLE param: data_type;

	BEGIN
		print_header(TraceFile);
		LOOP
		Instr := Memory(PC);
		OP := Instr / (2 ** reg_addr_width) ** 3; -- Anweisung lesen
		x := (Instr / (2 ** reg_addr_width) ** 2)MOD 2 ** reg_addr_width; -- Anweisung zerlegen
		y := (Instr / 2 ** reg_addr_width) MOD 2 ** reg_addr_width; -- Anweisung zerlegen
		z := Instr MOD 2 ** reg_addr_width; -- Anweisung zerlegen
		-- a muss noch initialisiert werden a:=

		write_PC_CMD(l, PC, Op, X, Y, Z); -- vor INC(PC)!!!

		PC := INC(PC);
 
		CASE OP IS -- Anweisungen differenzieren und ausführen
 
			-- Miscellaneous
			WHEN code_nop => NULL; -- keine Operation (3.3.1.1) 
				write_NoParam(l);
			WHEN code_stop => write_NoParam(l); -- stop Simulation (3.3.1.2)
			  write_regs (l, Reg(x), Reg(y), Reg(z), Reg(a));
		    writeline(TraceFile, l);
				print_dump(Memory, DumpFile);
				WAIT;
 
				-- ===============================================================================================================================================
				-- 
 
				-- ===============================================================================================================================================
				-- die OPCode Operationen hier einfügen (s.h. Vorlesung ?? Seite 49ff. - Statements for Arithmetic and Logic Ops)
				-- ===============================================================================================================================================

				-- Arithmetic
			WHEN code_add => ADD(Reg(y), Reg(z), Reg(x), Carry, Zero, Overflow);
				write_NoParam(l);
			WHEN code_addc => ADDC(Reg(y), Reg(z), Reg(x), Carry, Zero, Overflow);
				write_NoParam(l);
			WHEN code_sub => SUB(Reg(y), Reg(z), Reg(x), Zero, Negative);
				write_NoParam(l);
			WHEN code_subc => SUBC(Reg(y), Reg(z), Reg(x), Carry, Zero, Overflow, Negative);
				write_NoParam(l);
				-- Logical
			WHEN code_not => Reg(x) := "NOT"(Reg(y)); -- Verneinung (3.3.1.7)
				write_NoParam(l);
			WHEN code_and => Reg(x) := (Reg(y)) AND (Reg(z)); -- UND-Operation (3.3.1.8)
				write_NoParam(l);
			WHEN code_or => Reg(x) := (Reg(y)) OR (Reg(z)); -- OR-Operation (3.3.1.9)
				write_NoParam(l);
			WHEN code_xor => Reg(x) := (Reg(y)) XOR (Reg(z)); -- xor (3.3.1.10)
				write_NoParam(l);
			WHEN code_rea => REA(Reg(x), Reg(y));							-- rea (3.3.11)
				write_NoParam(l);
			WHEN code_reo => REO(Reg(x), Reg(y));							-- reo (3.3.12)
				write_NoParam(l);
			WHEN code_rex => REX(Reg(x), Reg(y));							-- rex (3.3.13)
				write_NoParam(l);
 
			-- Shift / Rotate
 			-- when code_sll        => XSLL(Reg(y),Reg(x),Zero,Carry,Negative,Overflow);
			-- when code_srl        => XSRL(Reg(y),Reg(x),Zero,Carry,Negative,Overflow);
			-- when code_sra        => XSRA(Reg(y),Reg(x),Zero,Carry,Negative,Overflow);
			-- when code_rol        => Carry := FALSE;
			-- 		        ROLC(Reg(y),Reg(x),Zero,Carry,Negative,Overflow);
			-- when code_rolc       => Carry := TRUE;
			-- 		        ROLC(Reg(y),Reg(x),Zero,Carry,Negative,Overflow);
			-- when code_ror        => Carry := FALSE;
			--                         RORC(Reg(y),Reg(x),Zero,Carry,Negative,Overflow); 
			-- when code_rorc       => Carry := TRUE;
			--                         RORC(Reg(y),Reg(x),Zero,Carry,Negative,Overflow);
 
			-- Memory Access
 			WHEN code_ldc => Reg(x):= Memory(PC);							-- ldc (3.3.21)
 				write_param(l, Memory(PC));
 			WHEN code_ldd => Reg(x):= Memory(Memory(PC));			-- ldc (3.3.22)
 				write_param(l, Memory(PC));
 			WHEN code_ldr => Reg(x):= Memory(Reg(Y));					-- ldc (3.3.23)
				write_NoParam(l);
 			WHEN code_std => Memory(Memory(PC)):= Reg(x);			-- ldc (3.3.24)
				write_param(l, Memory(PC));
 			WHEN code_str => Memory(Reg(Y)):=Reg(x);					-- ldc (3.3.25)
				write_NoParam(l);
 
			-- I/O
 			WHEN code_in => ReadIn(Reg(x));
 					write_NoParam(l);
 			WHEN code_out => WriteOut(Reg(x));
 					write_NoParam(l);
			-- Jump
 			WHEN code_jmp => PC := jmp(Memory(PC));
 				write_param(l, Memory(PC));
 			WHEN code_jz => PC := jz(Memory(PC), PC,zero);
 				write_param(l, Memory(PC));
 			WHEN code_jc => PC := jc(Memory(PC), PC, carry);
 				write_param(l, Memory(PC));
 			WHEN code_jn => PC := jc(Memory(PC), PC, negative);
 				write_param(l, Memory(PC));
 			WHEN code_jo => PC := jc(Memory(PC), PC, overflow);
 				write_param(l, Memory(PC));
 			WHEN code_jnz => PC := jc(Memory(PC), PC, zero);
 				write_param(l, Memory(PC));
 			WHEN code_jnc => PC := jc(Memory(PC), PC, carry);
 				write_param(l, Memory(PC));
 			WHEN code_jnn => PC := jc(Memory(PC), PC, negative);
 				write_param(l, Memory(PC));
 			WHEN code_jno => PC := jc(Memory(PC), PC, overflow);
 				write_param(l, Memory(PC));
 			

			WHEN OTHERS => -- ungültig oder bisher nicht implementiert
				ASSERT FALSE
				REPORT "ungültig"
					SEVERITY error;
		END CASE;
		write_regs (l, Reg(x), Reg(y), Reg(z), Reg(a));
		writeline(TraceFile, l);
	END LOOP;
END PROCESS;
END behav;
