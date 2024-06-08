module conversion;
import std.format;

string bytesToDefb(ubyte[] bytes)
{
    if (bytes.length == 0) return "";
    string data = "DEFB ";
    foreach (i, b; bytes)
    {
        data ~= "$" ~ format("%02X", b);
        if (i + 1 != bytes.length)
            data ~= ", ";
    }
    return data;
}

string escapeString(string input) {
    string result = "";
    foreach (char c; input) {
        switch (c) {
            case '\\':
                result ~= "\\\\";
                break;
            case '\"':
                result ~= "\\\"";
                break;
            // Add more cases for other special characters if needed
            default:
                result ~= c;
                break;
        }
    }
    return result;
}

string escapeString(const(ubyte[]) input) {
    string result = "";
    foreach (char c; input) {
        switch (c) {
            case '\\':
                result ~= "\\\\";
                break;
            case '\"':
                result ~= "\\\"";
                break;
            case '\0':
                result ~= "\\0";
                break;
            default:
                result ~= c;
                break;
        }
    }
    return result;
}