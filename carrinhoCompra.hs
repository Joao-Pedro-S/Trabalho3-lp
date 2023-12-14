module CarrinhoCompra where
import EstoqueProdutos

data CartItem = CartItem
  { cartItemProduto :: Produto
  , cartItemQuant :: Int
  } deriving (Show)

type Carrinho = [CartItem]

-- Função para adicionar produto ao carrinho de compras
addToCart :: Carrinho -> Estoque -> String -> Int -> Maybe Carrinho
addToCart cart estoque nome quant = do
  product <- findProduto estoque nome
  let atualCart = CartItem product quant : cart
  return atualCart

-- Função para encontrar produto no estoque, pesquisa por nome
findProduto :: Estoque -> String -> Maybe Produto
findProduto [] _ = Nothing
findProduto (produto:rest) nome
  | produtoNome produto == nome = Just produto
  | otherwise = findProduto rest nome

-- Função para processar o carrinho e atualizar o estoque. Usa a próxima função
processCart :: Carrinho -> Estoque -> Maybe (Carrinho, Estoque)
processCart cart estoque = do
  atualEstoqueList <- mapM processCartItem cart
  let atualEstoque = concat atualEstoqueList
  return (cart, atualEstoque)

-- Função para processar um unico item, chamada pela função anterior e utiliza fazerCompra de EstoqueProdutos
processCartItem :: CartItem -> Maybe Estoque
processCartItem (CartItem produto quant) =
  fazerCompra [produto] (produtoNome produto) quant

-- Função para calcular o custo total do carrinho de compras
calcTotal :: Carrinho -> Double
calcTotal cart = sum $ map calcItem cart
  where
    -- Função para calcular o custo de um dos itens do carrinho, levando em consideração a quantidade comprada
    calcItem :: CartItem -> Double
    calcItem (CartItem produto quant) = produtoCusto produto * fromIntegral quant