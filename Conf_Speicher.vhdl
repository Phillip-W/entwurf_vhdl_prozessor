configuration Conf_Speicher of test_Speicher
is
for TB

    for  
        UUT: Entity_Speicher
          use
             entity Entity_Speicher
                         (function_S);
    end for;

end for;
end Conf_Speicher;
