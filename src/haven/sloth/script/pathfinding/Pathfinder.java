package haven.sloth.script.pathfinding;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.resutil.CaveTile;
import haven.resutil.Ridges;
import haven.resutil.WaterTile;
import haven.sloth.DefSettings;
import haven.sloth.gob.HeldBy;
import haven.sloth.gob.Holding;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
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

    private final Coord start;
    private final Coord target;
    private final Gob targetgob;
    public final UI ui;

    private final Coord ul;
    private final Coord br;
    private Tile map[][];
    private int msz;

    public Pathfinder(final UI ui, final Coord2d start, final Coord2d target, final int viewdist) {
        this.ui = ui;
	this.start = start.floor();
	this.target = target.floor();
	this.targetgob = null;
	msz = viewdist * 2 * MCache.tilesz2.x;
	this.map = new Tile[msz][msz];
	final Coord view = new Coord(viewdist, viewdist).mul(MCache.tilesz2);
	this.ul = this.start.sub(view);
	this.br = this.start.add(view);
	snapshot();
    }

    public Pathfinder(final UI ui, final Coord2d start, final int viewdist, final Gob g) {
	this.ui = ui;
	this.start = start.floor();
	this.target = new Coord2d(g.getc()).floor();
	this.targetgob = g;
	msz = viewdist * 2 * MCache.tilesz2.x;
	this.map = new Tile[msz][msz];
	final Coord view = new Coord(viewdist, viewdist).mul(MCache.tilesz2);
	this.ul = this.start.sub(view);
	this.br = this.start.add(view);
	snapshot();
    }

    private Coord xlate(Coord c, Coord off) {
	c = c.sub(ul);
	return c.add(off);
    }

    private Coord tilify(Coord c) {
	c = c.div(MCache.tilesz2);
	c = c.mul(MCache.tilesz2);
	c = c.add(MCache.tilesz2.div(2));
	return c;
    }

    /**
     * For setting a tile to GOB
     */
    private void set(int x, int y, final Tile type) {
	if( x >= 0 && y >= 0 &&
		x < map.length &&
		y < map[0].length) {
	    map[x][y] = type;
	}
    }

    //Some modified Bresenham's line alg for steep slopes from a berkeley slide
    private void drawline(Coord c1, Coord c2, final Tile type) {
	drawline(c1.x, c1.y, c2.x, c2.y, type);
    }

    /**
     * Drawing the outline of our gob
     */
    private void drawline(int x0, int y0, int x1, int y1, final Tile type) {
	boolean steep = Math.abs(y1-y0) > Math.abs(x1-x0);
	if(steep) {
	    int t = x0;
	    x0 = y0;
	    y0 = t;

	    t = x1;
	    x1 = y1;
	    y1 = t;
	}
	if(x0 > x1) {
	    int t = x0;
	    x0 = x1;
	    x1 = t;

	    t = y0;
	    y0 = y1;
	    y1 = t;
	}

	float
		slope = ((float) y1-y0) / ((float) x1-x0),
		dErr = Math.abs(slope);
	int yStep = y0 > y1 ? -1 : 1;
	float err = 0.0f;
	int y = y0;

	for(int x = x0; x <= x1; ++x) {
	    if(steep)
		set(y,x,type);
	    else
		set(x,y,type);
	    err += dErr;
	    if(err >= 0.5f) {
		y += yStep;
		err -= 1.0;
	    }
	}
    }

    /**
     * Filling in the inner space of a gob that is at an angle.
     */
    private void fillspace(Coord start, final Tile type) {
	ArrayDeque<Coord> queue = new ArrayDeque<>();
	queue.push(start);

	Coord c;
	while(queue.size() > 0) {
	    c = queue.pop();
	    if(c.x >= 0 && c.y >= 0 && c.x < map.length && c.y < map[0].length &&
		    map[c.x][c.y] != type) {
		map[c.x][c.y] = type;
		queue.add(c.add(1,0));
		queue.add(c.add(0,1));
		queue.add(c.add(-1,0));
		queue.add(c.add(0,-1));
	    }
	}
    }

    private void fill(final Gob g, final Tile type) {
	final Hitbox hb = Hitbox.hbfor(g);
	if(hb != null) {
	    if(hb.canHit()) {
		final Coord hoff = new Coord(hb.offset());
		final Coord hsz = new Coord(hb.size());
		final double gd = Math.toDegrees(g.a);
		if(gd == 90 || gd == 270) {
		    //Easy case, simple rotations
		    int tmp = hoff.x;
		    hoff.x = hoff.y;
		    hoff.y = tmp;
		    tmp = hsz.x;
		    hsz.x = hsz.y;
		    hsz.y = tmp;
		} else if(gd != 0 && gd != 180) {
		    //some angle that's not trival
		    //idea: calculate the four corner points
		    //draw line between these four corner points
		    //then do a fill within these 4 lines that will fill any
		    //point not filled within it already and check for more until
		    //no more are found

		    //Only problem is making sure that initial point within the four lines
		    // is actually within (half-way point between two opposite corners to get
		    // center of rectangle hitbox)
		    Coord
			    tl = hoff,
			    tr = hoff.add(hsz.x, 0),
			    bl = hoff.add(0, hsz.y),
			    br = hoff.add(hsz);
		    //rotate
		    tl = tl.rot((float)g.a);
		    tr = tr.rot((float)g.a);
		    bl = bl.rot((float)g.a);
		    br = br.rot((float)g.a);

		    //translate back
		    final Coord gc = new Coord(g.getc());
		    tl = xlate(gc, tl);
		    tr = xlate(gc, tr);
		    bl = xlate(gc, bl);
		    br = xlate(gc, br);

		    //Draw lines
		    drawline(tl,tr,type);
		    drawline(tr,br,type);
		    drawline(bl,br,type);
		    drawline(tl,bl,type);

		    //Fill from center
		    final Coord center = new Coord( (tl.x + br.x)/2.0f, (tl.y+br.y)/2.0f);
		    fillspace(center,type);
		    return;
		}

		//Handle gd = 0, 90, 180 or 270
		Coord off = xlate(new Coord(g.getc()), hoff);
		Coord br = off.add(hsz);
		if(off.x > msz || off.y > msz || br.x < 0 || br.y < 0)
		    return;

		int x, y;
		for(x = Math.max(off.x, 0); x < Math.min(br.x, msz); ++x)
		    for(y = Math.max(off.y, 0); y < Math.min(br.y, msz); ++y)
			map[x][y] = Tile.GOB;
	    }
	} else {
	    logger.atWarning().log("No hitbox found for %s", g.resname());
	}
    }

    /**
     * Takes a snapshot of the surrounding area within view to figure out what we can and can't walk through
     */
    private void snapshot() {
        final Gob me = ui.sess.glob.oc.getgob(ui.gui.map.plgob);
        if(me != null) {
	    final boolean boating;
	    //Figure out if we're boating or not
	    //TODO: Technically this would classify on Wagons as well, we'll ignore that for now
	    boating = me.getattr(HeldBy.class) != null;

	    //Start by filling in the map tiles
	    final Coord end = tilify(br), start = tilify(ul);
	    if(!boating) {
		final Coord cliffoff = new Coord(-5, -5), cliffsz = new Coord(11,11);
		final Coord wateroff = new Coord(-5, -5), watersz = new Coord(11,11);
	        Coord xy = new Coord();
	        for(xy.x = start.x; xy.x != end.x; xy.x += 11) {
	            for(xy.y = start.y; xy.y != end.y; xy.y += 11) {
	                final int tileid = ui.sess.glob.map.gettile_safe(xy.div(MCache.tilesz2));
	                final Tiler tiler = ui.sess.glob.map.tiler(tileid);
	                if(tiler instanceof Ridges.RidgeTile && Ridges.brokenp(ui.sess.glob.map, xy.div(MCache.tilesz2))) {
	                    //entire tile is a ridge in our mindset
			    final Coord xl = xlate(xy, cliffoff);
			    for(int x = Math.max(xl.x, 0); x < Math.min(xl.x+cliffsz.x, msz); x++) {
			        for(int y = Math.max(xl.y, 0); y < Math.min(xl.y+cliffsz.y,msz); y++) {
			            map[x][y] = Tile.RIDGE;
				}
			    }
			} else if(tiler instanceof WaterTile) {
			    final Coord xl = xlate(xy, wateroff);
			    for(int x = Math.max(xl.x, 0); x < Math.min(xl.x+watersz.x, msz); x++) {
				for(int y = Math.max(xl.y, 0); y < Math.min(xl.y+watersz.y,msz); y++) {
				    map[x][y] = tileid != 173 && tileid != 97 ? Tile.DEEPWATER : Tile.SHALLOWWATER;
				}
			    }
			} else if(tiler instanceof CaveTile) {
			    final Coord xl = xlate(xy, wateroff);
			    for(int x = Math.max(xl.x, 0); x < Math.min(xl.x+watersz.x, msz); x++) {
				for(int y = Math.max(xl.y, 0); y < Math.min(xl.y+watersz.y,msz); y++) {
				    map[x][y] = Tile.CAVE;
				}
			    }
			}
		    }
		}
	    } else {
		final Coord wateroff = new Coord(-5, -5), watersz = new Coord(11,11);
		Coord xy = new Coord();
		for(xy.x = start.x; xy.x != end.x; xy.x += MCache.tilesz2.x) {
		    for(xy.y = start.y; xy.y != end.y; xy.y += MCache.tilesz2.y) {
			final int tileid = ui.sess.glob.map.gettile_safe(xy.div(MCache.tilesz2));
			final Tiler tiler = ui.sess.glob.map.tiler(tileid);
		        if(!(tiler instanceof WaterTile)) {
			    final Coord xl = xlate(xy, wateroff);
			    for(int x = Math.max(xl.x, 0); x < Math.min(xl.x+watersz.x, msz); x++) {
				for(int y = Math.max(xl.y, 0); y < Math.min(xl.y+watersz.y,msz); y++) {
				    //Odd to do it this way, but when boating DEEPWATER tiles will be bad to be consistent
				    //with how not boating works.
				    //null tiles will still be good ones just as above
				    map[x][y] = Tile.DEEPWATER;
				}
			    }
			}
		    }
		}
	    }

	    //Then gobs
	    synchronized (ui.sess.glob.oc) {
	        for(final Gob g : ui.sess.glob.oc) {
	            if(g.isDiscovered() && g.getattr(HeldBy.class) == null && (g.getattr(Holding.class) == null ||
			    g.getattr(Holding.class).held != me)) {
	                fill(g, Tile.GOB);
		    }
		}
	    }


	    // our own gob
	    fill(me, Tile.PLAYER);

	    if(DefSettings.DEBUG.get())
	        debug();
	}
    }

    private void debug() {
        final HashMap<Tile, Color> tmap = new HashMap<>();
	final BufferedImage buf = new BufferedImage(msz, msz, BufferedImage.TYPE_INT_RGB);
	tmap.put(Tile.GOB, Color.RED);
	tmap.put(Tile.DEEPWATER, Color.BLUE);
	tmap.put(Tile.SHALLOWWATER, Color.CYAN);
	tmap.put(Tile.CAVE, Color.GRAY);
	tmap.put(Tile.RIDGE, Color.YELLOW);
	tmap.put(Tile.PLAYER, Color.GREEN);

	int x,y;
	for(x=0;x<msz;++x)
	    for(y=0;y<msz;++y)
	        if(map[x][y] != null)
	            buf.setRGB(x,y,tmap.get(map[x][y]).getRGB());
	try {
	    javax.imageio.ImageIO.write(buf, "png", new File("hitmap.png"));
	} catch(Exception e) { e.printStackTrace(); }
	{
	    final Coord end = tilify(br), start = tilify(ul);
	    final BufferedImage buf2 = new BufferedImage((end.x - start.x) / MCache.tilesz2.x, (end.y - start.y) / MCache.tilesz2.x, BufferedImage.TYPE_INT_RGB);
	    Coord xy = new Coord();
	    for(xy.x = start.x; xy.x != end.x; xy.x += MCache.tilesz2.x) {
		for(xy.y = start.y; xy.y != end.y; xy.y += MCache.tilesz2.y) {
		    final Coord xy2 = xy.div(MCache.tilesz2);
		    if(ui.sess.glob.map.gethitmap(xy2) != null)
			buf2.setRGB((xy.x - start.x) / MCache.tilesz2.x, (xy.y - start.y) / MCache.tilesz2.y, tmap.get(ui.sess.glob.map.gethitmap(xy2)).getRGB());
		}
	    }
	    try {
		javax.imageio.ImageIO.write(buf2, "png", new File("hitmap3.png"));
	    } catch(Exception e) { e.printStackTrace(); }
	}
    }

    private void save(Coord start, List<Move> paths) {
	final HashMap<Tile, Color> tmap = new HashMap<>();
	final BufferedImage buf = new BufferedImage(msz, msz, BufferedImage.TYPE_INT_RGB);
	tmap.put(Tile.GOB, Color.RED);
	tmap.put(Tile.DEEPWATER, Color.BLUE);
	tmap.put(Tile.SHALLOWWATER, Color.CYAN);
	tmap.put(Tile.CAVE, Color.GRAY);
	tmap.put(Tile.RIDGE, Color.YELLOW);
	tmap.put(Tile.PLAYER, Color.GREEN);

	int x,y;
	for(x=0;x<msz;++x)
	    for(y=0;y<msz;++y)
	        if(map[x][y] != null)
	            buf.setRGB(x,y,tmap.get(map[x][y]).getRGB());
	        else
	            buf.setRGB(x,y, Color.WHITE.getRGB());

	final Color[] co = {
		Color.black, Color.cyan, Color.green,
		Color.magenta, Color.orange, Color.pink, Color.yellow
	};
	int i = 0, max = co.length;

	for(Move mv : paths) {
	    Coord c = mv.dest().floor();
	    Coord off = c.sub(start).add(msz/2,msz/2);
	    if(off.x >= 0 && off.y >= 0 && off.x+1 < msz && off.y+1 < msz) {
		buf.setRGB(off.x, off.y, co[i].getRGB());
		buf.setRGB(off.x, off.y + 1, co[i].getRGB());
		buf.setRGB(off.x + 1, off.y, co[i].getRGB());
		buf.setRGB(off.x + 1, off.y + 1, co[i].getRGB());
	    }
	    i = (i + 1) % max;
	}


	try {
	    javax.imageio.ImageIO.write(buf, "png", new File("hitmap2.png"));
	} catch(Exception e) { e.printStackTrace(); }
    }

    /**
     * Did we hit a bad spot at this coordinate?
     * Good spots are null or PLAYER
     */
    private boolean hit(Coord c) {
	c = xlate(c, plhb.offset());
	Coord br = c.add(plhb.size());
	if(c.x > msz || c.y > msz || br.x < 0 || br.y < 0)
	    return true;

	int x, y;
	for(x = Math.max(c.x, 0); x < Math.min(br.x, msz); ++x)
	    for(y = Math.max(c.y, 0); y < Math.min(br.y, msz); ++y)
		if(map[x][y] != null && map[x][y] != Tile.SHALLOWWATER && map[x][y] != Tile.PLAYER)
		    return true;
	return false;
    }

    public boolean walk(final Coord start, final Coord end) {
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
		    if(hit(new Coord(x,y)))
			return false;
		}
	    } else {
		dx = dx < 0 ? -1 : dx > 0 ? 1 : 0;
		int x, y;
		for(x = start.x; x != end.x; x += dx) {
		    y = (int) (slope * x + b);
		    if(hit(new Coord(x,y)))
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
		if(hit(new Coord(start.x,y)))
		    return false;
	    }
	}
	return true;
    }


    private ArrayList<Move> advreduce(List<Coord> lines) {
	final ArrayList<Move> blines = new ArrayList<>(lines.size());
	Coord cur, next;
	Coord best = null;
	int i,j, besti = 0;
	for(i=0;i<lines.size();++i) {
	    cur = lines.get(i);
	    for(j=i+1;j<lines.size();++j) {
		next = lines.get(j);
		if(walk(cur,next)){
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



    private Path findpath(Coord st, Coord goal, boolean allowbest, int level) {
	PriorityQueue<Path> pq = new PriorityQueue<> (msz*msz);
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
		if(!hit(nc)){
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
	switch(DefSettings.PATHFINDINGTIER.get()) {
	    case 1:
		Path ret = findpath(start, goal, allowbest, 0);
		if(ret != null) {
		    if(DefSettings.DEBUG.get())
			save(start, advreduce(ret.fullpath()));
		    return advreduce(ret.fullpath());
		}
		break;
	    case 2:
		path = findpath(start, goal, allowbest, 1);
		if(path != null) {
		    if(DefSettings.DEBUG.get())
			save(start, advreduce(path.fullpath()));
		    return advreduce(path.fullpath());
		}
		break;
	    case 3:
		//First pass
		path = findpath(start, goal, allowbest, 2);
		if(path != null) {
		    List<Coord> ipaths = path.fullpath();
		    ArrayList<Coord> fin = new ArrayList<>();
		    Coord next = ipaths.get(0);
		    int i;

		    //Second pass
		    for(i=0;i<ipaths.size()-2;++i) {
			path = findpath(next, ipaths.get(i+1), allowbest, 1);
			if(path != null) {
			    fin.addAll(path.fullpath());
			    next = fin.get(fin.size()-1);
			}
		    }
		    if(DefSettings.DEBUG.get())
			save(start, advreduce(fin));
		    return advreduce(fin);
		}
		break;
	}
	return null;
    }
}
