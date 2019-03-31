package haven.sloth.security;


public class Pad {
    //Prepends 'data' array with 'n' bytes, only defined for n > 0
    public static byte[] prepend(byte[] data, int n) {
        if (n <= 0)
            return data;

        byte[] ret = new byte[data.length + n];
        System.arraycopy(data, 0, ret, n, data.length);
        return ret;
    }

    //Appends 'data' array with 'n' bytes, only defined for n > 0
    public static byte[] append(byte[] data, int n) {
        if (n <= 0)
            return data;

        byte[] ret = new byte[data.length + n];
        System.arraycopy(data, 0, ret, 0, data.length);
        return ret;
    }

    //Removes the last 'n' bytes from data
    public static byte[] deappend(byte[] data, int n) {
        if (n <= 0)
            return data;

        byte[] ret = new byte[data.length - n];
        System.arraycopy(data, 0, ret, 0, ret.length);
        return ret;
    }

    //Removes the first 'n' bytes from data
    public static byte[] deprepend(byte[] data, int n) {
        if (n <= 0)
            return data;

        byte[] ret = new byte[data.length - n];
        System.arraycopy(data, n, ret, 0, ret.length);
        return ret;
    }
}
