----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/18/2016 04:29:01 PM
-- Design Name: 
-- Module Name: complex_multi - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity complex_multi is
	Port( clk : in STD_LOGIC;
       	  reset : in STD_LOGIC;
          hso: out std_logic;
          vso: out std_logic;
          red: out std_logic_vector(3 downto 0);
          blue:out std_logic_vector(3 downto 0);
          green:out std_logic_vector(3 downto 0)
          );
end complex_multi;

package complex_numbers is
	constant Length_complex : Double := 32;
	--32 bites complex
	type Complex32 is record
		R : Signed(Lenght_complex - 1 downto 0);
		I : Signed(Lenght_complex - 1 downto 0);
	end record;
	--64 bites complex
	type Complex64 is record
			R : Signed(2*Lenght_complex - 1 downto 0);
			I : Signed(2*Lenght_complex - 1 downto 0);
	end record;
end complex_numbers;
--body
package body complex_numbers is
	--32 bites összeadás túlterhelés
	function "+" (A: Complex32; 
				  B: Complex32) 
	return Complex32 is variable V: Complex32;
	begin
		V.R := A.R + B.R;
		V.I := A.I + B.I;
		return V;
	end "+";
	--64 bites összeadás túlterhelés
	function "+" (A: Complex64; 
				  B: Complex64) 
		return Complex64 is variable V: Complex64;
		begin
			V.R := A.R + B.R;
			V.I := A.I + B.I;
			return V;
	end "+";
	--szorzás túlterhelés
	function "*" (A: Complex32; 
				  B: Complex32)
		return Complex64 is variable V: Complex64;
		begin 
			V.R := A.R*B.R - (A.I*B.I);
			V.I := A.I*B.R + (A.R*B.I);
			return V;
	end "*";
end complex_numbers;

architecture Behavioral of complex_multi is



begin


end Behavioral;
