package haven.sloth.script.pathfinding;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.glsl.Array;
import haven.sloth.gob.HeldBy;

import java.util.*;

public class Pathfinder {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    protected static final Coord[][] dirs = new Coord[4][];
    protected static Hitbox plhb;
    static {
	plhb = Hitbox.hbfor("gfx/borka/body");

	final int x = plhb.size().x, y = plhb.size().y;
	//perfect
	dirs[0] = new Coord[8];
	dirs[0][0] = new Coord(1, 0);
	dirs[0][1] = new Coord(-1, 0);
	dirs[0][2] = new Coord(0, 1);
	dirs[0][3] = new Coord(0, -1);
	dirs[0][4] = new Coord(1, 1);
	dirs[0][5] = new Coord(1, -1);
	dirs[0][6] = new Coord(-1, -1);
	dirs[0][7] = new Coord(-1, 1);
	//half hitbox steps
	dirs[1] = new Coord[4];
	dirs[1][0] = new Coord(x/2, 0);
	dirs[1][1] = new Coord(-x/2, 0);
	dirs[1][2] = new Coord(0, y/2);
	dirs[1][3] = new Coord(0, -y/2);
	dirs[2] = new Coord[4];
	//hitbox sized steps, any bigger and i'd have to walk my path every move to ensure i didn't
	//jump over something bad
	dirs[2][0] = new Coord(x, 0);
	dirs[2][1] = new Coord(-x, 0);
	dirs[2][2] = new Coord(0, y);
	dirs[2][3] = new Coord(0, -y);
	//a mix of all three
	dirs[3] = new Coord[12];
	dirs[3][0] = dirs[2][0];
	dirs[3][1] = dirs[2][1];
	dirs[3][2] = dirs[2][2];
	dirs[3][3] = dirs[2][3];
	dirs[3][4] = dirs[1][0];
	dirs[3][5] = dirs[1][1];
	dirs[3][6] = dirs[1][2];
	dirs[3][7] = dirs[1][3];
	dirs[3][8] = dirs[0][0];
	dirs[3][9] = dirs[0][1];
	dirs[3][10] = dirs[0][2];
	dirs[3][11] = dirs[0][3];
    }

    @FunctionalInterface
    interface HitFun {
	boolean check(final Coord mc);
    }

    protected final UI ui;
    private final HitFun hitfun;

    public Pathfinder(final UI ui) {
	this.ui = ui;
	//Check to see if we're boating
	hitfun = areWeBoating() ? this::hitOnBoat : this::hitOnLand;
    }

    private boolean areWeBoating() {
	final Gob me = ui.sess.glob.oc.getgob(ui.gui.map.plgob);
	if(me != null) {
	    return me.getattr(HeldBy.class) != null;
	} else {
	    return false;
	}
    }

    /**
     * Did we hit a bad spot at this coordinate?
     * Good spots are null or PLAYER
     *
     * This is two checks
     * Are we on a bad tile and is our gob hitbox overlapping another
     */
    private boolean hitGob(final Coord mc) {
	final Coord c = mc.add(plhb.offset());
	final Coord br = c.add(plhb.size());

	Coord xy = new Coord(0, 0);
	for(xy.x = c.x; xy.x < br.x; ++xy.x)
	    for(xy.y = c.y; xy.y < br.y; ++xy.y)
		if (ui.sess.glob.gobhitmap.checkHit(xy))
		    return true;
	return false;
    }

    /**
     * In this case water tiles are safe, everything else = no no
     *
     * TODO: plhb is problem slightly too big for this since tiles will let you usually overlap a bit
     */
    private boolean hitOnBoat(final Coord mc) {
	final Coord c = mc.add(plhb.offset());
	final Coord br = c.add(plhb.size());

	Coord xy = new Coord(0, 0);
	for(xy.x = c.x; xy.x < br.x; ++xy.x)
	    for(xy.y = c.y; xy.y < br.y; ++xy.y) {
		final Tile t = ui.sess.glob.map.gethitmap(xy.div(MCache.tilesz2));
		if (t != Tile.DEEPWATER && t != Tile.SHALLOWWATER)
		    return true;
	    }
	return false;
    }

    /**
     * In this case everything is safe except for water and cave walls and ridges
     *
     * TODO: plhb is problem slightly too big for this since tiles will let you usually overlap a bit
     *       Especially the case in caves, not so much with ridges...
     */
    private boolean hitOnLand(final Coord mc) {
	final Coord c = mc.add(plhb.offset());
	final Coord br = c.add(plhb.size());

	Coord xy = new Coord(0, 0);
	for(xy.x = c.x; xy.x < br.x; ++xy.x)
	    for(xy.y = c.y; xy.y < br.y; ++xy.y)
		if(ui.sess.glob.map.gethitmap(xy.div(MCache.tilesz2)) != null)
		    return true;
	return false;
    }

    protected boolean checkHit(final Coord mc) {
	return hitGob(mc) || hitfun.check(mc);
    }

    /**
     * Walks a path between two points to see if we'll hit anything
     */
    protected boolean walk(final Coord start, final Coord end) {
	if(end.x - start.x != 0) {
	    final double slope = (double)(end.y-start.y)/(double)(end.x-start.x);
	    final double b = -(slope*start.x)+start.y;
	    double dx = end.x - start.x;
	    double dy = end.y - start.y;
	    if(Math.abs(dy) > Math.abs(dx)) {
		dy = dy < 0 ? -1 : dy > 0 ? 1 : 0;
		int x, y;
		for(y = start.y; y != end.y; y += dy) {
		    x = (int) ((y - b)/slope);
		    if(checkHit(new Coord(x,y)))
			return false;
		}
	    } else {
		dx = dx < 0 ? -1 : dx > 0 ? 1 : 0;
		int x, y;
		for(x = start.x; x != end.x; x += dx) {
		    y = (int) (slope * x + b);
		    if(checkHit(new Coord(x,y)))
			return false;
		}
	    }
	} else {
	    //our x's are the same
	    //walking north/south
	    int dy = end.y - start.y;
	    dy = Integer.compare(dy, 0);
	    int y;
	    for(y = start.y; y != end.y; y += dy) {
		if(checkHit(new Coord(start.x,y)))
		    return false;
	    }
	}
	return true;
    }

    protected List<Coord> collect(final Coord end, final Map<Coord, Coord> parent) {
        final ArrayList<Coord> moves = new ArrayList<>();
        moves.add(end);
        for(Coord next = parent.get(end); next != null; next = parent.get(next)) {
            moves.add(next);
	}
        //reverse start -> finish
	Collections.reverse(moves);
        return moves;
    }


    /**
     * Reduce the nodes we have into lines the end points will be our clicks
     * to walk the path
     *
     * TODO: this could be improved by trying farthest away first rather than closest. Even binary search
     */
    protected ArrayList<Move> advreduce(List<Coord> lines) {
        if(lines != null) {
	    final ArrayList<Move> blines = new ArrayList<>(lines.size());
	    Coord cur, next;
	    Coord best = null;
	    int i, j, besti = 0;
	    for (i = 0; i < lines.size(); ++i) {
		cur = lines.get(i);
		for (j = i + 1; j < lines.size(); ++j) {
		    next = lines.get(j);
		    if (walk(cur, next)) {
			best = next;
			besti = j;
		    }
		}
		if (best != null) {
		    blines.add(new Move(new Coord2d(best)));
		    i = besti;
		    best = null;
		} else {
		    blines.add(new Move(new Coord2d(cur)));
		}
	    }
	    return blines;
	} else {
            return null;
	}
    }

}
