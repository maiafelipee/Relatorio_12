
data Encomenda = Encomenda
  { destinatario :: String
  , peso :: Double
  , frete :: Double
  }

lerEncomenda :: Int -> IO Encomenda
lerEncomenda numero = do
  putStrLn ("\nDigite o destinatario da encomenda " ++ show numero ++ ":")
  nome <- getLine

  putStrLn "Digite o peso da encomenda (kg):"
  pesoEncomenda <- readLn

  putStrLn "Digite o valor do frete:"
  valorFrete <- readLn

  return (Encomenda nome pesoEncomenda valorFrete)

descrever :: Encomenda -> String
descrever encomenda =
  destinatario encomenda
  ++ " - Peso: " ++ show (peso encomenda)
  ++ " kg - Frete: R$ " ++ show (frete encomenda)

cabemNaVassoura :: Double -> [Encomenda] -> [Encomenda]
cabemNaVassoura limite =
  filter (\e -> peso e <= limite)

totalDeFretes :: [Encomenda] -> Double
totalDeFretes encomendas =
  sum (map frete encomendas)

main :: IO ()
main = do
  putStrLn "Digite o peso maximo suportado pela vassoura:"
  limite <- readLn

  encomenda1 <- lerEncomenda 1
  encomenda2 <- lerEncomenda 2
  encomenda3 <- lerEncomenda 3
  encomenda4 <- lerEncomenda 4

  let encomendas =
        [encomenda1, encomenda2, encomenda3, encomenda4]

  let permitidas = cabemNaVassoura limite encomendas

  putStrLn "\n--- Todas as encomendas ---"
  mapM_ putStrLn (map descrever encomendas)

  putStrLn "\n--- encomendas que cabem na vassoura ---"
  mapM_ putStrLn (map descrever permitidas)

  putStrLn "\n--- Total de fretes ---"
  print (totalDeFretes permitidas)

