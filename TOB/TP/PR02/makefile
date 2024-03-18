all: run

run: build
	java -cp bin allumettes.Jouer $(args)

clean:
	rm -r bin

build:
	javac -cp src ./src/allumettes/Jouer.java -d bin