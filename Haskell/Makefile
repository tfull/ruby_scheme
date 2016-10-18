BIN = bin
SRC = src
FILES = $(SRC)/Main.hs $(SRC)/Tokenizer.hs $(SRC)/Type.hs $(SRC)/Parser.hs $(SRC)/Evaluator.hs

$(BIN)/Main: $(FILES)
	ghc --make -outputdir $(BIN) -o $@ $^

.PHONY: clean
clean:
	rm $(BIN)/*.o $(BIN)/*.hi $(BIN)/Main
