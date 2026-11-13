data Cafe = Cafe String Double Int

descrever :: Cafe -> String
descrever (Cafe nome preco intensidade) =
  nome ++ " - R$ " ++ show preco

forca :: Cafe -> String
forca (Cafe nome preco intensidade)
  | intensidade >= 8 = "Forte"
  | intensidade >= 5 = "Equilibrado"
  | otherwise = "Suave"

precoDoPedido :: Bool -> Cafe -> Double
precoDoPedido leite (Cafe nome preco intensidade) =
  if leite
    then preco + 2.0
    else preco

lerCafe :: Int -> IO Cafe
lerCafe numero = do
  putStrLn ("Digite o nome do cafe " ++ show numero ++ ":")
  nome <- getLine

  putStrLn "Digite o preco:"
  preco <- readLn

  putStrLn "Digite a intensidade (1 a 10):"
  intensidade <- readLn

  return (Cafe nome preco intensidade)

main :: IO ()
main = do
  cafe1 <- lerCafe 1
  cafe2 <- lerCafe 2
  cafe3 <- lerCafe 3

  let cafes = [cafe1, cafe2, cafe3]

  putStrLn "\n--- cafe do dia ---"

  mapM_ putStrLn (map descrever cafes)

  putStrLn "\n--- intensidade ---"

  mapM_ putStrLn (map forca cafes)

  putStrLn "\n--- precos do primeiro cafe ---"

  putStrLn "Preco sem leite:"
  print (precoDoPedido False cafe1)

  putStrLn "Preco com leite:"
  print (precoDoPedido True cafe1)
