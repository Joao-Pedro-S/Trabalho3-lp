module Principal where
import EstoqueProdutos
import CarrinhoCompra


main :: IO ()
main = do
  let estoqueInicial = [] :: Estoque
      estoque = addProduto estoqueInicial "monitor" 500.0 100
      estoque2 = addProduto estoque "telefone" 150.0 300
      estoque3 = addProduto estoque2 "teclado" 70.0 50
      estoqueF = addProduto estoque3 "mouse" 50.0 50
      emptyCart = [] :: Carrinho
-- ^^^criação e preenchimento do estoque
  putStrLn "\nEstoque:"
  mapM_ print estoqueF
-- vvv preenchimento do carrinho de compras
  putStrLn "\nAdicionando produtos ao carrinho:"
  cart <- case addToCart emptyCart estoqueF "monitor" 2 of
    Just updatedCart -> do
      putStrLn "Adicionou 2 monitores ao carrinho."
      return updatedCart
    Nothing -> do
      putStrLn "Falhou em adicionar ao carrinho."
      return emptyCart

  cart2 <- case addToCart cart estoqueF "telefone" 5 of
    Just updatedCart -> do
      putStrLn "Adicionou 5 telefones ao carrinho."
      return updatedCart
    Nothing -> do
      putStrLn "Falhou em adicionar ao carrinho."
      return cart

  cartF <- case addToCart cart2 estoqueF "teclado" 2 of
    Just updatedCart -> do
      putStrLn "Adicionou 2 teclados ao carrinho."
      return updatedCart
    Nothing -> do
      putStrLn "Falhou em adicionar ao carrinho."
      return cart2
--cartF e estoqueF sao sempre as ultimas listas, para editar menos...
  putStrLn "\nProcessando o carrinho de compras:"
  case processCart cartF estoqueF of
    Just (newCart, newEstoque) -> do
      putStrLn "\nEstoque atualizado:"
      mapM_ print newEstoque
    Nothing -> putStrLn "Processamento falhou devido a falta de itens em estoque."

  let total = calcTotal cartF
  putStrLn $ "\nCusto total dos produtos no carrinho: R$" ++ show total