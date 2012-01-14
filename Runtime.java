public class Runtime {
    public static int[] string_to_null_array(String str) {
        int[] out = new int[str.length()+1];
        out[str.length()] = 0;
        for(int i = 0; i < str.length(); i++) {
            out[i] = str.codePointAt(i);
        }
        return out;
    }
}
