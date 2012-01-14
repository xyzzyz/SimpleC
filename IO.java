public class IO {
    public static void print_int(CInt i) {
        System.out.println(i.c);
    }

    public static void print_string(CPointer s) {
        CInt[] chars = (CInt[]) s.c;
        for(CInt c: chars) {
            if(c.c == 0) break;
            char ch = (char) c.c;
            System.out.print(ch);
        }
    }

    public static void print_char(CInt c) {
        char ch = (char) c.c;
        System.out.print(ch);
    }
}
