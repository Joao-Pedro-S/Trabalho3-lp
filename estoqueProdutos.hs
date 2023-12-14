module EstoqueProdutos where


data Produto = Produto
  { produtoNome :: String
  , produtoCusto :: Double
  , produtoQuant :: Int
  } deriving (Show)

type Estoque = [Produto]


-- Função para adicionar um produto ao estoque
addProduto :: Estoque -> String -> Double -> Int -> Estoque
addProduto estoque nome custo quant =
  Produto { produtoNome = nome, produtoCusto = custo, produtoQuant = quant } : estoque

-- Função para atualizar estoque após uma compra
fazerCompra :: Estoque -> String -> Int -> Maybe Estoque
fazerCompra [] _ _ = Nothing
fazerCompra (produto@(Produto nome custo atualQuant):rest) alvoNome quant
  | nome == alvoNome && quant <= atualQuant =
      Just (Produto nome custo (atualQuant - quant) : rest)
  | otherwise = fmap (produto :) (fazerCompra rest alvoNome quant)

