defmodule Libbtc_exTest do
  use ExUnit.Case

  describe "Testing key derivation" do
    test "- BIP32/44 key derivation - mainnet key" do
      path = "m/44'/0'/0'"
    
      root_key = "xprv9s21ZrQH143K3yXHTHAY63oQbd6VC39tnBTo4gzcijstYtWPteYFGnstkCPebNx61qz8waogCTFa3VmHehXK8Y4VrYYXGzDqPJ1se7NutPo"
      res_key = "xprv9yNGMacFz4SuLAq6BS3W54ZHLTe8JrJHxv14wT4YEW9TFQxVay53QN92cMYmuHh6UQ7dKhdtBKqagSbAUNxPu59729NFjrykoRzFcp9Qn57"	

      res = :libbtc_ex.derive(root_key, path)
      assert {:ok, res_key} == res     

    end
    
    test "- BIP32/44 key derivation - testnet key" do
      path = "m/44'/0'/0'"
    
      root_key = "tprv8ZgxMBicQKsPd3qjqavQinYmcJJ8KFASttKG5f2y8WYqvb5brSAnsH8tyMcrAusEeSXLDFtquVrEYaEcEzBzZPFzKzPxC3CQzcuGAQgS7Y9" 
      res_key ="tprv8gmfm1W4EQDwW7CApzME8eZHdNXoqG85GJHWh6rbQhvqw3ioDKqiyB4sVBFar94EXubgbznyhPhHK5EJrc7XJZe1W2KCYnpTWB8wvtYiurw"

      res = :libbtc_ex.derive(root_key, path)
      assert {:ok, res_key} == res     

    end
  
    test "invalid root_key" do
      path = "invalid"
    
      root_key = "tprv8ZgxMBicQKsPd3qjqavQinYmcJJ8KFASttKG5f2y8WYqvb5brSAnsH8tyMcrAusEeSXLDFtquVrEYaEcEzBzZPFzKzPxC3CQzcuGAQgS7Y9" 

      res = :libbtc_ex.derive(root_key, path)
      assert {:error, 'Key derivation error'} == res     

    end

    test "invalid path" do
      path = "invalid"
    
      root_key = "tprv8ZgxMBicQKsPd3qjqavQinYmcJJ8KFASttKG5f2y8WYqvb5brSAnsH8tyMcrAusEeSXLDFtquVrEYaEcEzBzZPFzKzPxC3CQzcuGAQgS7Y9" 

      res = :libbtc_ex.derive(root_key, path)
      assert {:error, 'Key derivation error'} == res     

    end


  end
end
