class TestCompiler {
    public static void main(String[] args) {
        CInt i = new CInt();
        i.c = 10;
        
        System.out.println(Test.test(i).c);
    }
}
