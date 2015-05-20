package subtype_package_all is
     
      subtype mem_data is integer range 4095 downto 0;  //Subtype für Speicher
      type mem_type is array(integer range 4095 downto 0) of integer range 4095 downto 0;  //type für Speicher (Array type)
       
end subtype_package_all;
