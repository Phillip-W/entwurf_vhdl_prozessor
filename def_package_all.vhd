LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
use std.textio.all;				

package def_package_all is

-- ===============================================================================================================================================      
  --Hier alle Konstanten definieren
-- ===============================================================================================================================================

  constant data_width       :positive :=12;   --1.1.1
  constant address_width    :positive :=12;   --1.1.2
  constant opcode_width     :positive :=6;    --3.1.1
  
  constant reg_addr_width   :positive :=2;    --2.1.2
	constant addr_width				:positive :=12;		-- nicht ändern
  

-- ===============================================================================================================================================  
  --Hier alle Datentypen, Subtyps definieren
-- ===============================================================================================================================================

  subtype addr_type IS												-- unsers Adressen zum ansprechen des Speichers, z.B. durch den PC
    natural range 0 to 2**address_width-1;
  subtype opcode_type IS											-- für unsere OPCode Deklarationen
    natural range 0 to 2**opcode_width-1;
  subtype reg_addr_type is										-- zum Ansprechen unserer Register
    natural range 0 to 2**reg_addr_width-1;  
  subtype data_type is 												-- Typ, wie unsere Anweisungen, etc. im Speicher abgelegt werden
    natural range 0 to 2**data_width-1;  
  type mem_type is 														-- unser "Speicher" (Array)
    array(addr_type) of data_type;  			
  type reg_type is 														-- 2.1.2.1 unsere "Register" (Array)
		array(reg_addr_type) of data_type;	


-- ===============================================================================================================================================  
  --Hier alle OPCodes definieren (vollständig)
-- ===============================================================================================================================================

  constant code_nop   : opcode_type:=0;    	    --3.3.1.1
  constant code_stop  : opcode_type:=1;     	  --3.3.1.2
  constant code_add   : opcode_type:=2;       	--3.3.1.3
	constant code_addc	: opcode_type:=3;					--3.3.1.4
	constant code_sub		: opcode_type:=4;					--3.3.1.5
	constant code_subc	: opcode_type:=5;					--3.3.1.6
	constant code_not		: opcode_type:=6;					--3.3.1.7
	constant code_and		: opcode_type:=7;					--3.3.1.8
  constant code_or    : opcode_type:=8;       	--3.3.1.9
  constant code_xor   : opcode_type:=9;       	--3.3.1.10
  constant code_rea   : opcode_type:=10;       	--3.3.1.11
	constant code_reo 	: opcode_type:=11;				--3.3.1.12
	constant code_rex 	: opcode_type:=12;				--3.3.1.13
	constant code_sll 	: opcode_type:=13;				--3.3.1.14
	constant code_srl 	: opcode_type:=14;				--3.3.1.15
	constant code_sra 	: opcode_type:=15;				--3.3.1.16
  constant code_rol   : opcode_type:=16;       	--3.3.1.17
  constant code_rolc  : opcode_type:=17;       	--3.3.1.18
  constant code_ror   : opcode_type:=18;       	--3.3.1.19
	constant code_rorc	: opcode_type:=19;				--3.3.1.20
	constant code_ldc 	: opcode_type:=32;				--3.3.1.21
	constant code_ldd 	: opcode_type:=33;				--3.3.1.22
	constant code_ldr 	: opcode_type:=34;				--3.3.1.23
	constant code_std		: opcode_type:=35;				--3.3.1.24
  constant code_str   : opcode_type:=36;       	--3.3.1.25
  constant code_in    : opcode_type:=37;       	--3.3.1.26
  constant code_out   : opcode_type:=38;       	--3.3.1.27
	constant code_jmp 	: opcode_type:=48;				--3.3.1.28
	constant code_jz		: opcode_type:=49;				--3.3.1.29
	constant code_jc  	: opcode_type:=50;				--3.3.1.30
	constant code_jn		: opcode_type:=51;				--3.3.1.31
	constant code_jo		: opcode_type:=52;				--3.3.1.32
	constant code_jnz		: opcode_type:=53;				--3.3.1.33
	constant code_jnc  	: opcode_type:=54;				--3.3.1.34
	constant code_jnn		: opcode_type:=55;				--3.3.1.35
	constant code_jn0		: opcode_type:=56;				--3.3.1.36     

	function INC (constant PC: addr_type)		-- PC-"increaser" (2.1.3.4; 2.1.3.3
		return addr_type; 


-- ===============================================================================================================================================
 -- die Funktionen / Proceduren für unsere OPCodes
-- ===============================================================================================================================================

	function "NOT" (constant A:data_type)
		return data_type;

	function "AND" (constant A,B:data_type)
		return data_type;

	function "OR" (constant A,B:data_type)
		return data_type;

	function "XOR" (constant A,B:data_type)
		return data_type;

	function CheckZeroFlag (constant Reg: data_type)
		return boolean; 

	procedure ADD (constant O1, O2: in data_type; R: inout data_type; C, Z, O: out boolean);

	procedure ADDC (constant O1, O2: in data_type; R: inout data_type;  C: inout boolean; Z, O: out boolean);

	procedure SUB (constant O1, O2: in data_type; R: inout data_type; Z, N: out boolean);

	procedure SUBC (constant O1, O2: in data_type; R: inout data_type; C: inout boolean; Z, O, N: out boolean);
	
	Procedure XSLL(constant A: in data_type; variable R: out data_type; variable Z,CO,N,O: out boolean);
				  
	procedure XSRL(constant A: in data_type; variable R: out data_type; variable Z,C,N,O: out boolean);
					
	Procedure XSRA(constant A: in data_type; variable R: out data_type; variable Z,CO,N,O: out boolean);
				  
	procedure ROLC(constant A: in data_type; variable R: out data_type; variable Z: out boolean; variable C: inout boolean; variable N,O: out boolean) ;
		
	procedure RORC(constant A: in data_type; variable R: out data_type; variable Z: out boolean; variable C: inout boolean; variable N,O: out boolean);


-- ===============================================================================================================================================
 -- die Proceduren / Funktionen für unser IO
-- ===============================================================================================================================================
	
	procedure print_tail (variable f:out text);

	procedure write_PC_CMD (variable l:inout line; constant PC: in data_type; constant OP: in opcode_type; constant x,y,z: in reg_addr_type);

	procedure print_header (variable f: out text);

	procedure write_param (variable l:inout line; constant param: in data_type);

	procedure write_NoParam (variable l: inout line);

	procedure write_regs (variable l: inout line; constant r0, r1, r2, r3 : in data_type );
  
	procedure write_flags (variable l: inout line; constant Zero, Carry, Negative, Overflow : in boolean );
  
  -- unsere transformationsfunktionen (hex_image, cmd_image, ...) fehlen noch.
      
end def_package_all;
