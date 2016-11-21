
--  Xilinx Simple Dual Port Single Clock RAM
--  This code implements a parameterizable SDP single clock memory.
--  If a reset or enable is not necessary, it may be tied off or removed from the code.
--  Modify the parameters for the desired RAM characteristics.

-- Following libraries have to be used
--use ieee.std_logic_1164.all;
--use std.textio.all;
--use ieee.numeric_std.all;


--Insert the following in the architecture before the begin keyword 
--  The following function calculates the address width based on specified RAM depth
function clogb2( depth : natural) return integer is
variable temp    : integer := depth;
variable ret_val : integer := 0; 
begin					
    while temp > 1 loop
        ret_val := ret_val + 1;
        temp    := temp / 2;     
    end loop;
    return ret_val;
end function;


-- Note : 
-- If the chosen width and depth values are low, Synthesis will infer Distributed RAM. 
-- C_RAM_DEPTH should be a power of 2
constant C_RAM_WIDTH : integer := <ram_width>;            		-- Specify RAM data width
constant C_RAM_DEPTH : integer := <ram_depth>; 				        -- Specify RAM depth (number of entries)              
constant C_RAM_PERFORMANCE : string := <ram_performance>; 		-- Select "HIGH_PERFORMANCE" or "LOW_LATENCY" 
constant C_INIT_FILE : string := <init_file>;    				 -- Specify name/location of RAM initialization file if using one (leave blank if not)       

signal <addra> : std_logic_vector((clogb2(C_RAM_DEPTH)-1) downto 0);     -- Write address bus, width determined from RAM_DEPTH
signal <addrb> : std_logic_vector((clogb2(C_RAM_DEPTH)-1) downto 0);     -- Read address bus, width determined from RAM_DEPTH
signal <dina>  : std_logic_vector(C_RAM_WIDTH-1 downto 0);		  -- RAM input data
signal <clka>  : std_logic;                       			  -- Clock
signal <wea>   : std_logic;                       			  -- Write enable
signal <enb>   : std_logic;                       			  -- RAM Enable, for additional power savings, disable port when not in use
signal <rstb>  : std_logic;                       			  -- Output reset (does not affect memory contents)
signal <regceb>: std_logic;                       			  -- Output register enable
signal <doutb> : std_logic_vector(C_RAM_WIDTH-1 downto 0); 		  -- RAM output data

signal <doutb_reg> : std_logic_vector(C_RAM_WIDTH-1 downto 0) := (others => '0');
type ram_type is array (C_RAM_DEPTH-1 downto 0) of std_logic_vector (C_RAM_WIDTH-1 downto 0);          -- 2D Array Declaration for RAM signal
signal <ram_data> : std_logic_vector(C_RAM_WIDTH-1 downto 0) ;

-- The folowing code either initializes the memory values to a specified file or to all zeros to match hardware
function initramfromfile (ramfilename : in string) return ram_type is
file ramfile	: text is in ramfilename;
variable ramfileline : line;
variable ram_name	: ram_type;
variable bitvec : bit_vector(C_RAM_WIDTH-1 downto 0);
begin
    for i in ram_type'range loop
        readline (ramfile, ramfileline);
        read (ramfileline, bitvec);
        ram_name(i) := to_stdlogicvector(bitvec);
    end loop;
    return ram_name;
end function;

function init_from_file_or_zeroes(ramfile : string) return ram_type is 
begin 
    if ramfile = "<Init File Name>" then 
        return InitRamFromFile("<Init File Name>") ; 
    else 
        return (others => (others => '0')); 
    end if; 
end;

-- Define RAM
signal <ram_name> : ram_type := init_from_file_or_zeroes(C_INIT_FILE);

--Insert the following in the architecture after the begin keyword
process(<clka>)
begin
    if(<clka>'event and <clka> = '1') then
        if(<wea> = '1') then
            <ram_name>(to_integer(unsigned(<addra>))) <= <dina>;
        end if;
        if(<enb> = '1') then
            <ram_data> <= <ram_name>(to_integer(unsigned(<addrb>)));
        end if;
    end if;
end process;

--  Following code generates LOW_LATENCY (no output register)
--  Following is a 1 clock cycle read latency at the cost of a longer clock-to-out timing

no_output_register : if C_RAM_PERFORMANCE = "LOW_LATENCY" generate
    <doutb> <= <ram_data>;
end generate;

--  Following code generates HIGH_PERFORMANCE (use output register) 
--  Following is a 2 clock cycle read latency with improved clock-to-out timing

output_register : if C_RAM_PERFORMANCE = "HIGH_PERFORMANCE"  generate
process(<clka>)
begin
    if(<clka>'event and <clka> = '1') then
        if(<rstb> = '1') then
            <doutb_reg> <= (others => '0');
        elsif(<regceb> = '1') then
            <doutb_reg> <= <ram_data>;
        end if;
    end if;
end process;
<doutb> <= <doutb_reg>;
end generate;

						
						
