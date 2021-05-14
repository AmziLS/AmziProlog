#all: hello.xpl Hello.class

run: hello.xpl Hello.class
	java -cp .:$(AMZI_DIR)/langbindings/java -Djava.library.path=".:$(AMZI_DIR)/lib" Hello
hello.xpl: hello.pro
	acmp hello
	alnk hello hello
Hello.class: Hello.java
	javac Hello.java -cp $(AMZI_DIR)/langbindings/java