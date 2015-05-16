package Package_MaxCPU is


  constant highest_natural : natural:= 500;
  constant bus_width : natural:= 11;
  constant address_width : natural:= 11;

  subtype LimitedWidthNatural is natural range 11 to highest_natural;

end Package_MaxCPU;



package body Package_MaxCPU is


end Package_MaxCPU;
