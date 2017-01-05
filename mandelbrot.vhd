library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity mandelbrot is
generic(  sizeX          : integer := 512;  
          sizeY          : integer := 512);
Port (    red       : out STD_LOGIC_VECTOR (3 downto 0) := "0000";
          green     : out STD_LOGIC_VECTOR (3 downto 0) := "0000";
          blue      : out STD_LOGIC_VECTOR (3 downto 0) := "0000";
          hs_o      : out STD_LOGIC;
          vs_o      : out STD_LOGIC;
          clk_board : in STD_LOGIC := '0';
          reset     : in STD_LOGIC := '0';
          led       : out STD_LOGIC_VECTOR (2 downto 0) := "000";
          start_btn : in STD_LOGIC := '0';
          disable   : in STD_LOGIC_VECTOR (2 downto 0) := "000";
          move_btn  : in STD_LOGIC_VECTOR (3 downto 0) := "0000";
          zoom_btn  : in STD_LOGIC := '0'
          );
end mandelbrot;

architecture Behavioral of mandelbrot is

component vga_clk_gen
port (
  clk_in1           : in     std_logic;
  clk_out1          : out    std_logic
 );
end component;

COMPONENT multi
  PORT (
    aclk : IN STD_LOGIC;
    s_axis_a_tvalid : IN STD_LOGIC;
    s_axis_a_tdata : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
    s_axis_b_tvalid : IN STD_LOGIC;
    s_axis_b_tdata : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
    m_axis_result_tvalid : OUT STD_LOGIC;
    m_axis_result_tdata : OUT STD_LOGIC_VECTOR(63 DOWNTO 0)
  );
END COMPONENT;

COMPONENT substract
  PORT (
    aclk : IN STD_LOGIC;
    s_axis_a_tvalid : IN STD_LOGIC;
    s_axis_a_tdata : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
    s_axis_b_tvalid : IN STD_LOGIC;
    s_axis_b_tdata : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
    m_axis_result_tvalid : OUT STD_LOGIC;
    m_axis_result_tdata : OUT STD_LOGIC_VECTOR(63 DOWNTO 0)
  );
END COMPONENT;

COMPONENT adder
  PORT (
    aclk : IN STD_LOGIC;
    s_axis_a_tvalid : IN STD_LOGIC;
    s_axis_a_tdata : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
    s_axis_b_tvalid : IN STD_LOGIC;
    s_axis_b_tdata : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
    m_axis_result_tvalid : OUT STD_LOGIC;
    m_axis_result_tdata : OUT STD_LOGIC_VECTOR(63 DOWNTO 0)
  );
END COMPONENT;

COMPONENT compare
  PORT (
    aclk : IN STD_LOGIC;
    s_axis_a_tvalid : IN STD_LOGIC;
    s_axis_a_tdata : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
    s_axis_b_tvalid : IN STD_LOGIC;
    s_axis_b_tdata : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
    m_axis_result_tvalid : OUT STD_LOGIC;
    m_axis_result_tdata : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
END COMPONENT;

----------------------------------------------------------------------------------------------------------------
-- Escape radius and its valid signal
--
signal nx : STD_LOGIC_VECTOR(63 DOWNTO 0):= X"4010000000000000" ;
--signal nx_valid : std_logic:='0'; --nem használjuk

constant MAX_ITER  : integer := 256-1;

signal zr : STD_LOGIC_VECTOR(63 DOWNTO 0) := (others => '0');
signal zi : STD_LOGIC_VECTOR(63 DOWNTO 0) := (others => '0');
signal zr_valid : std_logic:='0';
signal zi_valid : std_logic:='0';


signal cr : STD_LOGIC_VECTOR(63 DOWNTO 0);
signal ci : STD_LOGIC_VECTOR(63 DOWNTO 0);
signal cr_valid : std_logic:= '0';
signal ci_valid : std_logic:= '0';

-- z*z+c for the next iteration
signal mr,mi : STD_LOGIC_VECTOR(63 DOWNTO 0) := (others => '0');

--cr, ci mentes bal felso sarok
signal mcr,mci : STD_LOGIC_VECTOR(63 DOWNTO 0) := X"c000000000000000";
signal mcr_valid,mci_valid : std_logic:= '0';

--  move
signal next_mcr_r_result,next_mci_u_result : STD_LOGIC_VECTOR(63 DOWNTO 0) := X"c000000000000000";
signal next_mcr_r_valid,next_mci_u_valid : std_logic:= '0';

signal next_mcr_l_result,next_mci_d_result : STD_LOGIC_VECTOR(63 DOWNTO 0) := X"c000000000000000";
signal next_mcr_l_valid,next_mci_d_valid : std_logic:= '0';


--Number of iterations
signal t : unsigned(11 downto 0) :=  (others => '0');

-- Calculating r*r, i*i and r*i of the input z which is a complex number r+i*(i)
signal mult1_result: STD_LOGIC_VECTOR(63 DOWNTO 0);
signal mult2_result: STD_LOGIC_VECTOR(63 DOWNTO 0);
signal mult3_result: STD_LOGIC_VECTOR(63 DOWNTO 0);
signal ki_mult1_valid: std_logic:='0';
signal ki_mult2_valid: std_logic:='0';
signal ki_mult3_valid: std_logic:='0';

-- result of calculation: r*r-i*i

signal sub_result: STD_LOGIC_VECTOR(63 DOWNTO 0);
signal ki_sub_valid: std_logic:='0';


signal add1_result: STD_LOGIC_VECTOR(63 DOWNTO 0);
signal ki_add1_valid: std_logic:='0';

signal add2_r_result:STD_LOGIC_VECTOR(63 DOWNTO 0);
signal add2_i_result:STD_LOGIC_VECTOR(63 DOWNTO 0);
signal ki_add2_r_valid:std_logic:='0';
signal ki_add2_i_valid:std_logic:='0';

signal add3_result:STD_LOGIC_VECTOR(63 DOWNTO 0);
signal ki_add3_valid:std_logic:='0';

signal ki_mult4_valid:std_logic:='0';
signal mult4_result:STD_LOGIC_VECTOR(63 DOWNTO 0);

signal mult5_result:STD_LOGIC_VECTOR(63 DOWNTO 0);
signal ki_mult5_valid:std_logic:='0';

signal comparator_result:STD_LOGIC_VECTOR(7 DOWNTO 0);
signal ki_comparator_valid:std_logic:='0';


----------------------------------------------------------------------------------------------------------------
-- cr and ci of the next pixel
--
signal next_i_result:STD_LOGIC_VECTOR(63 DOWNTO 0);
signal next_r_result:STD_LOGIC_VECTOR(63 DOWNTO 0);

signal next_pixel_cr:STD_LOGIC_VECTOR(63 DOWNTO 0);
signal next_pixel_ci:STD_LOGIC_VECTOR(63 DOWNTO 0);

signal next_i_valid:std_logic:='0';
signal next_r_valid:std_logic:='0';



type allapot is (var,zoom,move,start,iter,itervege,t_null,sorvege,nextiter);
signal state, next_state :allapot;

----------------------------------------------------------------------------------------------------------------
-- Definition of RAM and its signals
--
constant C_RAM_WIDTH  : integer := 12;            		                  -- Specify RAM data width
constant C_RAM_DEPTH  : integer := 512*512;                               -- Specify RAM depth (number of entries) 

signal addra	      : std_logic_vector(18-1 downto 0);                  -- RAM input data address
signal wea            : std_logic;                       			      -- Write enable
signal memory_in_data : std_logic_vector(C_RAM_WIDTH-1 downto 0); 		  -- RAM input data
signal color_of_pixel : std_logic_vector(C_RAM_WIDTH-1 downto 0) ;        -- RAM output data

type ram_type is array (C_RAM_DEPTH-1 downto 0) of std_logic_vector (C_RAM_WIDTH-1 downto 0);          -- 2D Array Declaration for RAM signal
signal framebuffer : ram_type;


----------------------------------------------------------------------------------------------------------------
--XGA Signal 1024 x 768 @ 60 Hz timing - (65.0 MHz Pixel freq.)
--
--horizontal timings (pixels)
constant hor_visual      : integer := 1024;
constant hor_front_porch : integer := 24;
constant hor_sync        : integer := 136;
constant hor_back_porch  : integer := 160;

--vertical timings (lines)
constant ver_visual      : integer := 768;
constant ver_front_porch : integer := 3;
constant ver_sync        : integer := 6;
constant ver_back_porch  : integer := 29;

--VGA sync and sync counters
signal hs, vs       : STD_LOGIC := '0';
signal hsc          : unsigned(11 downto 0) := (others => '0');
signal vsc          : unsigned(10 downto 0) := (others => '0');
--signal tmp          : STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
--signal tmp_H        : STD_LOGIC_VECTOR(5 downto 0) := (others => '0');
--signal tmp_L        : STD_LOGIC_VECTOR(5 downto 0) := (others => '0');
signal enable_out : STD_LOGIC := '0';
signal enable_drawout : STD_LOGIC := '0';


----------------------------------------------------------------------------------------------------------------
-- 29s shift register signals for delays
--
type shift_reg_type is array (29-1 downto 0) of std_logic_vector (64-1 downto 0);

signal shift_r : shift_reg_type;
signal shift_i : shift_reg_type;
signal shift_v : STD_LOGIC_VECTOR(29-1 downto 0);

----------------------------------------------------------------------------------------------------------------
-- current pixel coordinates on the 512x512 screen
-- 
signal px, py 			: unsigned(8 downto 0) := (others => '0'); 

-- delta value between pixels
signal delta 			: STD_LOGIC_VECTOR(63 DOWNTO 0) := X"3f80000000000000"; -- 4/512
signal delta_valid_r	: std_logic:='0';
signal delta_valid_i	: std_logic:='0';

signal move_delta       : STD_LOGIC_VECTOR(63 DOWNTO 0) := X"3ff0000000000000";
signal move_r_valid	    : std_logic:='0';
signal move_l_valid   	: std_logic:='0';
signal move_d_valid	    : std_logic:='0';
signal move_u_valid	    : std_logic:='0';



--varas allapotba menjunk
signal go_to_var        : std_logic:='0';
signal exit_zoom_to_var : std_logic:='0';
signal sorvegen         : std_logic:='0';


signal timer:unsigned(27 downto 0);


-- 65 MHz clock signal
signal clk : STD_LOGIC;

begin

SYNC_PROC: process (clk)
begin
	if (clk'event and clk = '1') then
		if (reset = '1') then
			state <= var;
		else
			state <= next_state;
		end if;        
	end if;
end process;

OUTPUT_DECODE: process (clk)
begin
if( clk'event and clk = '1') then
	if (state = var) then
	    led(0) <= '1';
	    go_to_var <= '0';
	else
	    led(0) <= '0';
	    go_to_var <= go_to_var;
	end if;
	
	if (state = start) then
        cr <= X"c000000000000000"; --cr_bal_felso; --meg m�g a nagy�t�si faktor
        ci <= X"c000000000000000"; --ci_bal_felso;
        --mcr <= move_r_result;
        --mci <= move_i_result;
    elsif (state = itervege) then
    	cr <= next_pixel_cr;
        ci <= ci;
    elsif (state = sorvege) then
        cr <= mcr;
        ci <= next_pixel_ci;
    elsif (state = var) then
        cr <= mcr;
        ci <= mci;
--    elsif (state = move) then
--        if(move_btn(0) = '1' or move_btn(1) = '1') then 
--            ci <= next_mcr;
--        else
--            cr <=
--        end if;
    else
        mcr <= mcr;
        mci <= mci;
        cr <= cr;
        ci <= ci;
    end if;
	
	if (state = start or state = nextiter or state = t_null) then
        zr_valid <= '1';
        zi_valid <= '1';
	else
        zr_valid <= '0';
        zi_valid <= '0';  	   
	end if;
	
    if (state = start or state = nextiter or state = t_null or state = sorvege) then
        cr_valid <= '1';
        ci_valid <= '1';
    else
        cr_valid <= '0';
        ci_valid <= '0';     
    end if;
	
	if (state = nextiter) then
        zr <= mr;
        zi <= mi;
    elsif (state = start or state = t_null) then
        zr <= X"0000000000000000";
        zi <= X"0000000000000000";
    else
        zr <= zr;
        zi <= zi;
    end if;
    
    if ( state = zoom ) then
        if(timer = 0) then   
            delta(63 downto 52) <=  std_logic_vector( unsigned(delta(63 downto 52)) +1 );
        end if; 
        if(timer = 65_000_000 - 1) then 
           exit_zoom_to_var <= '1';    
           timer <= (others => '0');
        else
           exit_zoom_to_var <= '0';
           timer <= timer+1;
        end if;   
    else
        delta <= delta;
    end if;
	
	if (state = start or state = t_null) then
		delta_valid_r <= '1';
	else
		delta_valid_r <= '0';
	end if;
	
	if (state = start or state = sorvege) then
        delta_valid_i <= '1';
    else
        delta_valid_i <= '0';
    end if;
	
	
	if ( state = sorvege ) then

	   if(py = sizeY-1)then
           py <= (others => '0');
       else
           py <= py + 1;    
       end if;
    elsif  (state = var) then
       py <= (others => '0');
    else
       py <= py;
	end if;
		
	if (state = iter) then
        --sz�molgatunk...
        --sz�molgatunk...
	end if;
	
	if (state = itervege) then
	    wea <= '1';
	    memory_in_data <= std_logic_vector(t);
		--memory_in_data(11 downto 8) <= std_logic_vector(t(3 downto 0));
		--memory_in_data(7 downto 4)  <= std_logic_vector(t(3 downto 0));	
		--memory_in_data(3 downto 0)  <= std_logic_vector(t(3 downto 0));
		addra <= std_logic_vector(px) & std_logic_vector(py);

		if(px = sizeX-1) then
		    px <= (others => '0');
		else
			px <= px+1;
		end if;
	elsif  (state = var) then
 	   wea <= '0';
       px <= (others => '0');
	else
	   wea <= '0';
	   px <= px;
	end if;
	
	if(px = sizeX-1) then
	   sorvegen <= '1';
	else
	   sorvegen <= '0';
	end if;
	
	if( state = move ) then
        led(2) <= '1';
    else
        led(2) <= '0';
    end if;
	
	
    if(px = sizeX-1 and py = sizeY-1) then
       go_to_var <= '1'; 
    else
       go_to_var <= '0';
    end if;
	 
	if (state = nextiter) then
	    t <= t+1;
	elsif( state = start or  state = t_null) then
        t <= (others => '0');
	else
	    t <= t;
	end if;

end if;
end process;

NEXT_STATE_DECODE: process (state, start_btn, ki_comparator_valid, comparator_result, t, go_to_var, sorvegen, zoom_btn, move_btn)
begin
next_state <= state;
 case (state) is
	--inicializáljuk az első pixel helyét
	when var =>
	   if  (start_btn = '1') then
		  next_state <= start;
	   elsif(zoom_btn = '1') then
          next_state <= zoom;
       elsif(unsigned(move_btn) > 0) then
          next_state <= move;
	   else
	      next_state <= var;
	   end if;
						  
	when start =>
		next_state <= iter;
					  
	when iter =>
		if (ki_comparator_valid = '1') then
			if(comparator_result(0) = '1' and  t < MAX_ITER) then
			    next_state <= nextiter;
			--end if;
		    --if (comparator_result(0) = '0' or t >= 255) then
		    else
			    next_state <= itervege;
			end if;
		else
		    next_state <= iter;
		end if;
		 
	when nextiter =>
		next_state <= iter;
	
	when sorvege =>
        next_state <= t_null;
		
    when t_null =>
        next_state <= iter;
            
    when itervege =>
		if (go_to_var = '1') then
			next_state <= var;
		elsif (sorvegen = '1') then
		    next_state <= sorvege; 
		else
			next_state <= t_null;
		end if;
		
	when zoom =>
	    if (exit_zoom_to_var = '1') then
            next_state <= var;
	    else
            next_state <= zoom;
        end if;
        
    when move =>
        next_state <= var;
 end case;      
end process;

--RAM      
memoria: process(clk)
begin
    if(clk'event and clk = '1') then
        if(wea = '1') then
            framebuffer(to_integer(unsigned(addra))) <= memory_in_data;
        --elsif(reset = '1') then
         --    framebuffer(to_integer(unsigned(hsc(8 downto 0) & vsc(8 downto 0)))) <= (others => '1');
        end if;
        if(enable_drawout = '1') then 
            color_of_pixel <= framebuffer(to_integer(unsigned(hsc(8 downto 0) & vsc(8 downto 0))));
        else
            color_of_pixel <= (others => '1');
        end if;
    end if;
end process;

mentes_next: process(clk)
begin
	if( clk'event and clk = '1') then
		if(next_r_valid = '1') then
			next_pixel_cr <= next_r_result;
			next_pixel_ci <= next_i_result;
		else
			next_pixel_cr <= next_pixel_cr;
			next_pixel_ci <= next_pixel_ci;
		end if;	  		
	end if;
end process;


shiftreg: process(clk)
begin
	if( clk'event and clk = '1') then
		shift_r <= cr & shift_r(shift_r'left downto 1);
		shift_i <= ci & shift_i(shift_i'left downto 1);
		shift_v <= cr_valid & shift_v(shift_v'left downto 1);		
	end if;
end process;

mentes: process(clk)
begin
	if( clk'event and clk = '1') then
		if(ki_add2_r_valid = '1') then
			mr <= add2_r_result;
			mi <= add2_i_result;
		else
			mr <= mr;
			mi <= mi;
		end if;	  		
	end if;
end process;

enable_drawout_proc: process(clk, reset)
begin
   if (reset = '1') then
       enable_drawout <= '0';
   elsif (clk'event and clk='1') then
       if( hsc = 0 and vsc < sizeY-1) then
           enable_drawout <= '1';
       elsif(hsc = sizeX-1) then
           enable_drawout <= '0';
       else
           enable_drawout <= enable_drawout;
       end if;
   end if;
end process;


enable_out_proc: process(clk, reset)
begin
   if (reset = '1') then
       enable_out <= '0';
   elsif (clk'event and clk='1') then
       if(hsc = 0) then
           enable_out <= '1';
       elsif(hsc = hor_visual or vsc = ver_visual) then
           enable_out <= '0';
       else
           enable_out <= enable_out;
       end if;
   end if;
end process;

--fancy test pattern for VGA output
--tmp_H <= std_logic_vector(hsc(6 downto 3)) & std_logic_vector(vsc(5 downto 4));
--tmp_L <= std_logic_vector(vsc(6 downto 1)) XOR  ( std_logic_vector(vsc(5 downto 0)) AND  std_logic_vector(hsc(6 downto 1)) );
--tmp   <= tmp_H & tmp_L;

--kirajzolás
pattern: process(clk)
begin
    if (clk'event and clk='1') then
        if( enable_out = '1' ) then
            if (disable(0) = '1' ) then
                red   <= (others => '0');
            else
                red   <= color_of_pixel(11 downto 8);
            end if;
            if (disable(1) = '1') then
                green  <= (others => '0');
            else
                green  <= color_of_pixel(7 downto 4);
            end if;
            if (disable(2) = '1') then
                blue <= (others => '0');
            else
                blue <= color_of_pixel(3 downto 0);
            end if;
        else
            red   <= (others => '0');
            green <= (others => '0');
            blue  <= (others => '0');
        end if;
    end if;
end process;

vga_clk_mapper: vga_clk_gen
port map ( 
   clk_in1 => clk_board, 
   clk_out1 => clk              
);

hs_o <= hs;
vs_o <= vs;

counter_HS: process(clk)
begin
    if (clk'event and clk='1') then
        if (hsc = hor_visual+hor_front_porch-1) then
            hs <= '0';
        elsif (hsc = hor_visual+hor_front_porch+hor_sync-1) then
            hs <= '1';
        else
            hs <= hs;
        end if;
        if (hsc = hor_visual+hor_front_porch+hor_sync+hor_back_porch-1) then
            hsc <= (others => '0');
        else
            hsc <= hsc + 1;
        end if;
    end if;
end process;

counter_VS: process(clk)
begin
    if (clk'event and clk='1') then
        if (hsc = hor_visual+hor_front_porch+hor_sync+hor_back_porch-1) then 
            if (vsc = ver_visual+ver_front_porch-1) then
                vs <= '0';
            elsif (vsc = ver_visual+ver_front_porch+ver_sync-1) then
                vs <= '1';
            else
                vs <= vs;
            end if;
            if (vsc = ver_visual+ver_front_porch+ver_sync+ver_back_porch-1) then
                vsc <= (others => '0');
            else
                vsc <= vsc + 1;
            end if;
        end if;
    end if;
end process;

----------------------------------------------------------------------------------------------------------------
-- Portmap definitons for floating point operations
-- 

--z.r négyzet
mult1: multi
  PORT MAP (
    aclk => clk,
    s_axis_a_tvalid => zr_valid,
    s_axis_a_tdata => zr,
    s_axis_b_tvalid => zr_valid,
    s_axis_b_tdata => zr,
    m_axis_result_tvalid => ki_mult1_valid,
    m_axis_result_tdata => mult1_result
  );
  
--z.i négyzet 
mult2: multi
    PORT MAP (
      aclk => clk,
      s_axis_a_tvalid => zi_valid,
      s_axis_a_tdata => zi,
      s_axis_b_tvalid => zi_valid,
      s_axis_b_tdata => zi,
      m_axis_result_tvalid => ki_mult2_valid,
      m_axis_result_tdata => mult2_result
   );
   
--zr*z.i
mult3: multi
     PORT MAP (
       aclk => clk,
       s_axis_a_tvalid => zr_valid,
       s_axis_a_tdata => zr,
       s_axis_b_tvalid => zi_valid,
       s_axis_b_tdata => zi,
       m_axis_result_tvalid => ki_mult3_valid,
       m_axis_result_tdata => mult3_result
 );
 
--r*r - i*i 
sub: substract
   PORT MAP (
     aclk => clk,
     s_axis_a_tvalid => ki_mult1_valid,
     s_axis_a_tdata => mult1_result,
     s_axis_b_tvalid => ki_mult2_valid,
     s_axis_b_tdata => mult2_result,
     m_axis_result_tvalid =>ki_sub_valid,
     m_axis_result_tdata => sub_result
   );

--duplázó
add1: adder
     PORT MAP (
       aclk => clk,
       s_axis_a_tvalid => ki_mult3_valid,
       s_axis_a_tdata => mult3_result,
       s_axis_b_tvalid => ki_mult3_valid,
       s_axis_b_tdata => mult3_result,
       m_axis_result_tvalid => ki_add1_valid,
       m_axis_result_tdata => add1_result
     );

add2_r: adder
     PORT MAP (
       aclk => clk,
       s_axis_a_tvalid => ki_sub_valid,
       s_axis_a_tdata => sub_result,
       s_axis_b_tvalid => shift_v(0),
       s_axis_b_tdata => shift_r(0),
       m_axis_result_tvalid => ki_add2_r_valid,
       m_axis_result_tdata => add2_r_result
     );
     
add2_i: adder
          PORT MAP (
            aclk => clk,
            s_axis_a_tvalid => ki_add1_valid,
            s_axis_a_tdata => add1_result,
            s_axis_b_tvalid => shift_v(0),
            s_axis_b_tdata => shift_i(0),
            m_axis_result_tvalid => ki_add2_i_valid,
            m_axis_result_tdata => add2_i_result
          );     

-- real of z^2+c
mult4: multi
  PORT MAP (
    aclk => clk,
    s_axis_a_tvalid => ki_add2_r_valid,
    s_axis_a_tdata => add2_r_result,
    s_axis_b_tvalid => ki_add2_r_valid,
    s_axis_b_tdata => add2_r_result,
    m_axis_result_tvalid => ki_mult4_valid,
    m_axis_result_tdata => mult4_result
  );
  
  
-- imag of z^2+c
mult5: multi
    PORT MAP (
      aclk => clk,
      s_axis_a_tvalid => ki_add2_i_valid,
      s_axis_a_tdata => add2_i_result,
      s_axis_b_tvalid => ki_add2_i_valid,
      s_axis_b_tdata => add2_i_result,
      m_axis_result_tvalid => ki_mult5_valid,
      m_axis_result_tdata => mult5_result
    );


add3: adder
	  PORT MAP (
		aclk => clk,
		s_axis_a_tvalid => ki_mult4_valid,
		s_axis_a_tdata => mult4_result,
		s_axis_b_tvalid => ki_mult5_valid,
		s_axis_b_tdata => mult5_result,
		m_axis_result_tvalid => ki_add3_valid,
		m_axis_result_tdata => add3_result
	 );  
	 
comparator: compare
	   PORT MAP (
	     aclk => clk,
	     s_axis_a_tvalid => ki_add3_valid,
	     s_axis_a_tdata => add3_result,
	     s_axis_b_tvalid => ki_add3_valid,
	     s_axis_b_tdata => nx,
	     m_axis_result_tvalid => ki_comparator_valid,
	     m_axis_result_tdata => comparator_result
	   );
	   
	   
delta_adder_r: adder
		PORT MAP (
		  aclk => clk,
		  s_axis_a_tvalid => delta_valid_r,
		  s_axis_a_tdata => delta,
		  s_axis_b_tvalid => cr_valid,
		  s_axis_b_tdata => cr,
		  m_axis_result_tvalid => next_r_valid,
		  m_axis_result_tdata => next_r_result
		);

delta_adder_i: adder
		PORT MAP (
		  aclk => clk,
		  s_axis_a_tvalid => delta_valid_i,
		  s_axis_a_tdata => delta,
		  s_axis_b_tvalid => ci_valid,
		  s_axis_b_tdata => ci,
		  m_axis_result_tvalid => next_i_valid,
		  m_axis_result_tdata => next_i_result 
		);

-- jobbra
move_delta_adder_r: adder
        PORT MAP (
          aclk => clk,
          s_axis_a_tvalid => move_r_valid,
          s_axis_a_tdata => move_delta,
          s_axis_b_tvalid => mcr_valid,
          s_axis_b_tdata => mcr,
          m_axis_result_tvalid => next_mcr_r_valid,
          m_axis_result_tdata => next_mcr_r_result 
        );

-- balra
move_delta_substracter_r: substract
        PORT MAP (
          aclk => clk,
          s_axis_a_tvalid => move_l_valid,
          s_axis_a_tdata => move_delta,
          s_axis_b_tvalid => mcr_valid,
          s_axis_b_tdata => mcr,
          m_axis_result_tvalid => next_mcr_l_valid,
          m_axis_result_tdata => next_mcr_l_result 
        );

-- lefele
move_delta_adder_i: adder
        PORT MAP (
          aclk => clk,
          s_axis_a_tvalid => move_d_valid,
          s_axis_a_tdata => move_delta,
          s_axis_b_tvalid => mci_valid,
          s_axis_b_tdata => mci,
          m_axis_result_tvalid => next_mci_d_valid,
          m_axis_result_tdata => next_mci_d_result 
        );

-- felfele
move_delta_substracter_i: substract
        PORT MAP (
          aclk => clk,
          s_axis_a_tvalid => move_u_valid,
          s_axis_a_tdata => move_delta,
          s_axis_b_tvalid => mci_valid,
          s_axis_b_tdata => mci,
          m_axis_result_tvalid => next_mci_u_valid,
          m_axis_result_tdata => next_mci_u_result 
        );


end Behavioral;
