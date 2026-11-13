
data Esboco = Esboco
  { titulo :: String
  , tecnica :: String
  , horas :: Int
  }

lerEsboco :: Int -> IO Esboco
lerEsboco numero = do
  putStrLn ("\ndigite o titulo do esboco " ++ show numero ++ ":")
  nome <- getLine

  putStrLn "digite a tecnica utilizada:"
  tecnicaEsboco <- getLine

  putStrLn "digite quantas horas foram gastas:"
  horasEsboco <- readLn

  return (Esboco nome tecnicaEsboco horasEsboco)

esbocoMaisRecente :: [Esboco] -> String
esbocoMaisRecente [] = "O caderno esta vazio."
esbocoMaisRecente (e:_) =
  "Titulo: " ++ titulo e
  ++ " - Tecnica: " ++ tecnica e

contarEsbocos :: [Esboco] -> Int
contarEsbocos [] = 0
contarEsbocos (_:resto) =
  1 + contarEsbocos resto

horasDesenhando :: [Esboco] -> Int
horasDesenhando [] = 0
horasDesenhando (e:resto) =
  horas e + horasDesenhando resto

main :: IO ()
main = do
  esboco1 <- lerEsboco 1
  esboco2 <- lerEsboco 2
  esboco3 <- lerEsboco 3

  let caderno =
        [esboco1, esboco2, esboco3]

  putStrLn "\n--- CADERNO ORIGINAL ---"

  putStrLn (esbocoMaisRecente caderno)

  putStrLn ("Quantidade de esbocos: " ++ show (contarEsbocos caderno))

  putStrLn ("total de horas: " ++ show (horasDesenhando caderno))

  putStrLn "\n--- caderno vazio ---"

  putStrLn (esbocoMaisRecente [])

  putStrLn "\n--- novo esboco ---"

  novoEsboco <- lerEsboco 4

  let novoCaderno = novoEsboco : caderno

  putStrLn "\n--- comparacao dos cadernos ---"

  putStrLn "Caderno original:"
  putStrLn (esbocoMaisRecente caderno)
  putStrLn ("Quantidade: " ++ show (contarEsbocos caderno))

  putStrLn "\nNovo caderno:"
  putStrLn (esbocoMaisRecente novoCaderno)
  putStrLn ("Quantidade: " ++ show (contarEsbocos novoCaderno))

  -- head [] causaria erro:
  -- putStrLn (titulo (head []))

