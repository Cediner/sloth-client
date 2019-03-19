package haven.sloth.script.pathfinding;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.sloth.DefSettings;
import haven.sloth.gob.HeldBy;

import java.util.*;
import java.util.List;


public class Pathfinder {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Coord[][] dirs = new Coord[3][4];
    private static Hitbox plhb;
    static {
	plhb = Hitbox.hbfor("gfx/borka/body");

	final int x = plhb.size().x, y = plhb.size().y;
	dirs[0][0] = new Coord(1, 0);
	dirs[0][1] = new Coord(-1, 0);
	dirs[0][2] = new Coord(0, 1);
	dirs[0][3] = new Coord(0, -1);
	dirs[1][0] = new Coord(x, 0);
	dirs[1][1] = new Coord(-x, 0);
	dirs[1][2] = new Coord(0, y);
	dirs[1][3] = new Coord(0, -y);
	dirs[2][0] = new Coord(x * 2, 0);
	dirs[2][1] = new Coord(-x * 2, 0);
	dirs[2][2] = new Coord(0, y * 2);
	dirs[2][3] = new Coord(0, -y * 2);
    }

    @FunctionalInterface
    interface HitFun {
        boolean check(final Coord mc);
    }

    private final Coord start;
    private final Coord target;
    private final Gob targetgob;
    public final UI ui;
    private boolean boating;

    public Pathfinder(final UI ui, final Coord2d start, final Coord2d target) {
        this.ui = ui;
	this.start = start.floor();
	this.target = target.floor();
	this.targetgob = null;

	//Check to see if we're boating
	boating = areWeBoating();
    }

    public Pathfinder(final UI ui, final Coord2d start, final Gob g) {
	this.ui = ui;
	this.start = start.floor();
	this.target = new Coord2d(g.getc()).floor();
	this.targetgob = g;
	//Check to see if we're boating
	boating = areWeBoating();
    }

    private boolean areWeBoating() {
        final Gob me = ui.sess.glob.oc.getgob(ui.gui.map.plgob);
        if(me != null) {
            return me.getattr(HeldBy.class) != null;
	} else {
	    return false;
	}
    }

    private Coord tilify(Coord c) {
	c = c.div(MCache.tilesz2);
	c = c.mul(MCache.tilesz2);
	c = c.add(MCache.tilesz2.div(2));
	return c;
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

    private boolean checkHit(final Coord mc, final HitFun hit) {
	return hitGob(mc) || hit.check(mc);
    }

    public boolean walk(final Coord start, final Coord end, final HitFun hit) {
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
		    if(checkHit(new Coord(x,y), hit))
			return false;
		}
	    } else {
		dx = dx < 0 ? -1 : dx > 0 ? 1 : 0;
		int x, y;
		for(x = start.x; x != end.x; x += dx) {
		    y = (int) (slope * x + b);
		    if(checkHit(new Coord(x,y), hit))
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
		if(checkHit(new Coord(start.x,y), hit))
		    return false;
	    }
	}
	return true;
    }


    private ArrayList<Move> advreduce(List<Coord> lines, final HitFun hit) {
	final ArrayList<Move> blines = new ArrayList<>(lines.size());
	Coord cur, next;
	Coord best = null;
	int i,j, besti = 0;
	for(i=0;i<lines.size();++i) {
	    cur = lines.get(i);
	    for(j=i+1;j<lines.size();++j) {
		next = lines.get(j);
		if(walk(cur,next,hit)){
		    best = next;
		    besti = j;
		}
	    }
	    if(best != null){
		blines.add(new Move(new Coord2d(best)));
		i = besti;
		best = null;
	    } else {
		blines.add(new Move(new Coord2d(cur)));
	    }
	}
	return blines;
    }



    private Path findpath(Coord st, Coord goal, boolean allowbest, int level, final HitFun hit) {
	PriorityQueue<Path> pq = new PriorityQueue<> ();
	HashMap<Coord, Path> memo = new HashMap<>();
	Path cur;
	Path best;
	pq.add((best = new Path(st, goal)));

	while(pq.size() > 0) {
	    cur = pq.poll();
	    //check if goal
	    if(cur.c.equals(goal))
		return cur;
	    if(cur.gval < best.gval)
		best = cur;
	    //add successors
	    for(Coord s : dirs[level]) {
		Coord nc = cur.c.add(s);
		if(!checkHit(nc, hit)){
		    Path np = new Path(nc, cur, goal);
		    if(memo.get(nc) == null || memo.get(nc).hval > np.hval) {
			memo.put(np.c, np);
			pq.add(np);
		    }
		}
	    }
	}

	if(allowbest) {
	    if(best.c.equals(st))
		return new Path(goal, null, goal);
	    return new Path(goal, best, goal);
	}
	return null;
    }


    public ArrayList<Move> path(Coord goal, boolean allowbest) {
	Path path;
	final HitFun hit = !boating ? this::hitOnLand : this::hitOnBoat;
	switch(DefSettings.PATHFINDINGTIER.get()) {
	    case 1:
		Path ret = findpath(start, goal, allowbest, 0, hit);
		if(ret != null) {
		    return advreduce(ret.fullpath(), hit);
		}
		break;
	    case 2:
		path = findpath(start, goal, allowbest, 1, hit);
		if(path != null) {
		    return advreduce(path.fullpath(), hit);
		}
		break;
	    case 3:
		//First pass
		path = findpath(start, goal, allowbest, 2, hit);
		if(path != null) {
		    List<Coord> ipaths = path.fullpath();
		    ArrayList<Coord> fin = new ArrayList<>();
		    Coord next = ipaths.get(0);
		    int i;

		    //Second pass
		    for(i=0;i<ipaths.size()-2;++i) {
			path = findpath(next, ipaths.get(i+1), allowbest, 1, hit);
			if(path != null) {
			    fin.addAll(path.fullpath());
			    next = fin.get(fin.size()-1);
			}
		    }
		    return advreduce(fin, hit);
		}
		break;
	}
	return null;
    }
}
