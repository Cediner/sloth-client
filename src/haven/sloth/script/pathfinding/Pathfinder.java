package haven.sloth.script.pathfinding;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.glsl.Array;
import haven.sloth.DefSettings;
import haven.sloth.gob.HeldBy;
import haven.sloth.gob.Type;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.*;
import java.util.List;

public class Pathfinder {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    static final Coord[][] dirs = new Coord[1][4];
    private static Hitbox plhb;

    static {
        plhb = Hitbox.hbfor("gfx/borka/body");

        //perfect
        dirs[0][0] = new Coord(1, 0);
        dirs[0][1] = new Coord(-1, 0);
        dirs[0][2] = new Coord(0, 1);
        dirs[0][3] = new Coord(0, -1);
    }

    @FunctionalInterface
    interface HitFun {
        boolean check(final Coord mc);
    }

    @FunctionalInterface
    interface HeuristicFun {
        double distance(final Coord c, final Coord goal);
    }

    protected final UI ui;
    private final HitFun hitfun;
    final HeuristicFun heuristic;

    Pathfinder(final UI ui) {
        this.ui = ui;
        //Check to see if we're boating
        hitfun = areWeBoating() ? this::hitOnBoat : this::hitOnLand;
        heuristic = this::manhattanDistance;
    }

    //D and D2 can scale based off terrain/speed we can run, future something to look at maybe.
    private static final double D = 1;
    private static final double D2 = Math.sqrt(2);

    private double diagonalDistance(final Coord c, final Coord goal) {
        final double dx = Math.abs(c.x - goal.x);
        final double dy = Math.abs(c.y - goal.y);
        return D * (dx + dy) + (D2 - 2 * D) * Math.min(dx, dy);
    }

    private double manhattanDistance(final Coord c, final Coord goal) {
        final double dx = Math.abs(c.x - goal.x);
        final double dy = Math.abs(c.y - goal.y);
        return D * (dx + dy);
    }

    private boolean areWeBoating() {
        final Gob me = ui.sess.glob.oc.getgob(ui.gui.map.plgob);
        if (me != null) {
            return me.getattr(HeldBy.class) != null && me.getattr(HeldBy.class).holder.type == Type.WATERVEHICLE;
        } else {
            return false;
        }
    }

    /**
     * Did we hit a bad spot at this coordinate?
     * Good spots are null or PLAYER
     * <p>
     * This is two checks
     * Are we on a bad tile and is our gob hitbox overlapping another
     */
    private boolean hitGob(final Coord mc) {
        final Coord c = mc.add(plhb.offset());
        final Coord br = c.add(plhb.size());

        Coord xy = new Coord(0, 0);
        for (xy.x = c.x; xy.x < br.x; ++xy.x)
            for (xy.y = c.y; xy.y < br.y; ++xy.y)
                if (ui.sess.glob.gobhitmap.checkHit(xy))
                    return true;
        return false;
    }

    /**
     * In this case water tiles are safe, everything else = no no
     * <p>
     * TODO: plhb is problem slightly too big for this since tiles will let you usually overlap a bit
     */
    private boolean hitOnBoat(final Coord mc) {
        final Coord c = mc.add(plhb.offset());
        final Coord br = c.add(plhb.size());

        Coord xy = new Coord(0, 0);
        for (xy.x = c.x; xy.x < br.x; ++xy.x)
            for (xy.y = c.y; xy.y < br.y; ++xy.y) {
                final Tile t = ui.sess.glob.map.gethitmap(xy.div(MCache.tilesz2));
                if (t != Tile.DEEPWATER && t != Tile.SHALLOWWATER)
                    return true;
            }
        return false;
    }

    /**
     * In this case everything is safe except for water and cave walls and ridges
     * <p>
     * TODO: plhb is problem slightly too big for this since tiles will let you usually overlap a bit
     * Especially the case in caves, not so much with ridges...
     */
    private boolean hitOnLand(final Coord mc) {
        final Coord c = mc.add(plhb.offset());
        final Coord br = c.add(plhb.size());

        Coord xy = new Coord(0, 0);
        for (xy.x = c.x; xy.x < br.x; ++xy.x)
            for (xy.y = c.y; xy.y < br.y; ++xy.y)
                if (ui.sess.glob.map.gethitmap(xy.div(MCache.tilesz2)) != null)
                    return true;
        return false;
    }

    final boolean checkHit(final Coord mc) {
        return hitGob(mc) || hitfun.check(mc);
    }

    /**
     * Walks a path between two points to see if we'll hit anything
     */
    final protected boolean walk(final Coord start, final Coord end) {
        if (end.x - start.x != 0) {
            final double slope = (double) (end.y - start.y) / (double) (end.x - start.x);
            final double b = -(slope * start.x) + start.y;
            double dx = end.x - start.x;
            double dy = end.y - start.y;
            if (Math.abs(dy) > Math.abs(dx)) {
                dy = dy < 0 ? -1 : dy > 0 ? 1 : 0;
                int x, y;
                for (y = start.y; y != end.y; y += dy) {
                    x = (int) ((y - b) / slope);
                    if (checkHit(new Coord(x, y)))
                        return false;
                }
            } else {
                dx = dx < 0 ? -1 : dx > 0 ? 1 : 0;
                int x, y;
                for (x = start.x; x != end.x; x += dx) {
                    y = (int) (slope * x + b);
                    if (checkHit(new Coord(x, y)))
                        return false;
                }
            }
        } else {
            //our x's are the same
            //walking north/south
            int dy = end.y - start.y;
            dy = Integer.compare(dy, 0);
            int y;
            for (y = start.y; y != end.y; y += dy) {
                if (checkHit(new Coord(start.x, y)))
                    return false;
            }
        }
        return true;
    }

    final List<Coord> collect(final Node end) {
        final ArrayList<Coord> moves = new ArrayList<>();
        moves.add(end.c);
        for (Node next = end.parent; next != null; next = next.parent) {
            moves.add(next.c);
        }
        //reverse start -> finish
        Collections.reverse(moves);
        return moves;
    }


    private void debugl(List<Coord> lines) {
        if(lines.size() > 0) {
            //find our boundaries
            Coord tl = new Coord(lines.get(0));
            Coord br = new Coord(tl);
            for (final Coord c : lines) {
                if (c.x < tl.x)
                    tl.x = c.x;
                else if (c.x > br.x)
                    br.x = c.x;

                if (c.y < tl.y)
                    tl.y = c.y;
                else if (c.y > br.y)
                    br.y = c.y;
            }
            final BufferedImage buf = ui.sess.glob.gobhitmap.debug2(tl, br);

            for(final Coord c : lines) {
                final Coord offset = c.sub(tl);
                buf.setRGB(offset.x, offset.y, Color.WHITE.getRGB());
            }

            try {
                javax.imageio.ImageIO.write(buf, "png", new File("beforereduce.png"));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private void debug(List<Move> moves) {
        if(moves.size() > 0) {
            //find our boundaries
            Coord tl = new Coord(moves.get(0).dest().floor());
            Coord br = new Coord(tl);
            for (final Move m : moves) {
                final Coord c = m.dest().floor();
                if (c.x < tl.x)
                    tl.x = c.x;
                else if (c.x > br.x)
                    br.x = c.x;

                if (c.y < tl.y)
                    tl.y = c.y;
                else if (c.y > br.y)
                    br.y = c.y;
            }
            final BufferedImage buf = ui.sess.glob.gobhitmap.debug2(tl, br);
            final Graphics g = buf.createGraphics();
            g.setColor(Color.GREEN);

            for (int i = 0; i < moves.size(); ++i) {
                final Coord offset = moves.get(i).dest().floor().sub(tl);
                if(i + 1 < moves.size()) {
                    final Coord off2 = moves.get(i+1).dest().floor().sub(tl);
                    g.drawLine(offset.x, offset.y, off2.x, off2.y);
                }
                buf.setRGB(offset.x, offset.y, Color.WHITE.getRGB());
            }

            try {
                javax.imageio.ImageIO.write(buf, "png", new File("postreduce.png"));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Reduce the nodes we have into lines the end points will be our clicks
     * to walk the path
     * <p>
     * TODO: this could be improved by trying farthest away first rather than closest. Even binary search
     * <p>
     * In a way this tries to improve our result since it operates with the assumption that our List<Coord>
     * may not be as optimal as we think or not optimal in the sense of how many clicks we have to do
     * more clicks -> Slowdown -> bad and we'd rather have long straight lines rather than many short
     *
     * In the end we'll have
     *
     * Start -> X, X -> Y, Y -> Z, ..., U -> Goal
     *
     * Our pathfinder guarantees that X -> X+1 is SAFE, but not X -> X+n where n >= 2
     */
    final ArrayList<Move> advreduce(List<Coord> lines) {
        if (lines != null) {
            if(DefSettings.DEBUG.get())
                debugl(lines);
            final ArrayList<Move> blines = new ArrayList<>(lines.size());
            for (int i = 0; i < lines.size() - 1; ++i) {
                //Find the best line that goes from cur -> X
                //Best is judged based on how far along our points we can go before we hit something
                final Coord start = lines.get(i);
                int best = i + 1; //we know i+1 is safe
                for (int j = i + 2; j < lines.size(); ++j) {
                    if (walk(start, lines.get(j))) {
                        best = j;
                    }
                }

                //Our line is now start -> best
                blines.add(new Move(new Coord2d(lines.get(best))));
                //The next line should start from `best`
                i = best-1;
            }
            if(DefSettings.DEBUG.get())
                debug(blines);
            return blines;
        } else {
            return null;
        }
    }

    /**
     * Simple reduction of our points into lines
     * @param points List of points
     * @return A list of movement commands
     */
    final ArrayList<Move> simplereduce(List<Coord> points) {
        if(points != null) {
            if(DefSettings.DEBUG.get())
                debugl(points);
            final ArrayList<Move> moves = new ArrayList<>(points.size());
            Coord start, end;
            double slope;
            int i, j;
            for (i = 0; i < points.size()-1; ++i) {
                start = points.get(i);
                end = points.get(i+1);
                slope = (double)(end.y - start.y)/(double)(end.x - start.x);
                //Keep going until our vector changes directions
                for(j = i + 2; j < points.size(); ++j) {
                    final Coord nend = points.get(j);
                    if(slope == ((double)(nend.y - start.y)/(double)(nend.x - start.x))) {
                        end = nend;
                    } else
                        break;
                }
                i = j - 2; // The next line starts where we ended, i++ will make this j-1 which is end
                moves.add(new Move(new Coord2d(end)));
            }

            //Make sure we didn't not account for our ending point
            if(!moves.get(moves.size()-1).dest().equals(new Coord2d(points.get(points.size()-1)))) {
                moves.add(new Move(new Coord2d(points.get(points.size()-1))));
            }

            if(DefSettings.DEBUG.get())
                debug(moves);
            return moves;
        } else {
            return null;
        }
    }
}
