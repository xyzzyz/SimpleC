public class Runtime {
    public static CInt[] string_to_null_array(String str) {
        CInt[] out = new CInt[str.length()+1];
        CInt n = new CInt();
        n.c = 0;
        out[str.length()] = n;
        for(int i = 0; i < str.length(); i++) {
            CInt c = new CInt();
            c.c = str.codePointAt(i);
            out[i] = c;
        }
        return out;
    }
}
