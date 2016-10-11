BIN = bin
SRC = src

$(BIN)/Main: $(SRC)/Main.hs $(SRC)/Tokenizer.hs
	ghc --make -outputdir $(BIN) -o $@ $^
