.PHONY: clean
clean:
	rm -rf .stack-work

.PHONY: build
build:
	stack build

.PHONY: test
test:
	stack test

.PHONY: paydemo
paydemo:
	stack exec paydemo-exe

.PHONY: doubledeposit
doubledeposit:
	stack exec doubledeposit-exe

.PHONY: doublepayment
doublepayment:
	stack exec doublepayment-exe
