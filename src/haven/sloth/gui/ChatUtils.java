package haven.sloth.gui;

import haven.Coord;
import haven.Coord2d;
import haven.Coord3f;
import haven.Widget;
import haven.sloth.script.Context;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

public class ChatUtils {
    //Dispatch messages from External sources: (dispatch subject [...args])
    //where an arg will be: (type values...)
    //Support arg types:
    //  string,
    //  number,
    //  real
    //  coord,
    //  coord3f,
    //  coord2d
    //Ex: (dispatch lift-to (number 48214892912) (coord2d 4212.4, 4215.3))
    // -> Lift gob 48214892912 to (4212.4, 4215.3)
    public static final Pattern CHAT_EXT_MSG_PAT = Pattern.compile("\\(dispatch ([a-zA-Z0-9\\-]+) (.+)\\)");


    private static String nextToken(final String argstr, final int[] ref) {
        boolean inquotes = false;
        final int start;
        int i;
        findend:
        {
            //skip a space if present
            if (argstr.charAt(ref[0]) == ' ') {
                start = ref[0] + 1;
            } else {
                start = ref[0];
            }

            //figure out boundries
            for (i = start; i < argstr.length(); ++i) {
                switch (argstr.charAt(i)) {
                    case '"':
                        inquotes = !inquotes;
                        break;
                    case ' ':
                        if (!inquotes) {
                            break findend;
                        }
                        break;
                    case ')':
                        break findend;
                }
            }
        }
        //i @ ')' or ' '
        ref[0] = i;

        //Will only happen if missing a value expected for a type
        //ie: (coord 2)
        if (start == i) {
            throw new RuntimeException("Invalid dispatch argument - " + argstr + " starting at " + start);
        }

        return argstr.substring(start, i);
    }

    private static Object parseArg(final String argstr, final int[] ref) {
        final String type = nextToken(argstr, ref);
        switch (type) {
            case "string":
                return nextToken(argstr, ref);
            case "number":
                return Long.parseLong(nextToken(argstr, ref));
            case "real":
                return Double.parseDouble(nextToken(argstr, ref));
            case "coord":
                return new Coord(Integer.parseInt(nextToken(argstr, ref)),
                        Integer.parseInt(nextToken(argstr, ref)));
            case "coord3f":
                return new Coord3f(Float.parseFloat(nextToken(argstr, ref)),
                        Float.parseFloat(nextToken(argstr, ref)),
                        Float.parseFloat(nextToken(argstr, ref)));
            case "coord2d":
                return new Coord2d(Double.parseDouble(nextToken(argstr, ref)),
                        Double.parseDouble(nextToken(argstr, ref)));
            default:
                throw new RuntimeException("Invalid dispatch message - " + argstr);
        }
    }

    public static void parseExternalCommand(final Widget sender, final String subject, final String rawargstr) {
        final List<Object> args = new ArrayList<>();
        //remove dup spaces down to just 1, trim leading/following spaces, and get rid of spaces after ( and before )
        final String argstr = rawargstr
                .replaceAll("\\s\\s+", " ")
                .trim()
                .replaceAll("\\(\\s", "(")
                .replaceAll("\\s\\)", ")");
        //parse the argstr into arguments
        //The string should look like: `(type values...) [(type values ...)...])` where [..] is optional
        final int[] ref = new int[1];
        for (int i = 0; i < argstr.length(); ++i) {
            if (argstr.charAt(i) == '(') {
                ref[0] = i + 1;
                args.add(parseArg(argstr, ref));
                i = ref[0]; //i should now be at the ending `)`

                if (argstr.charAt(i) != ')') {
                    throw new RuntimeException("Invalid Dispatch command " + argstr);
                }
            }
        }

        //Dispatch the message if everything went well
        Context.dispatchmsg(sender, subject, args.toArray(new Object[0]));
    }
}
