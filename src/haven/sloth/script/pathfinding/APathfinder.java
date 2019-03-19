package haven.sloth.script.pathfinding;

import com.google.common.flogger.FluentLogger;
import haven.Coord;
import haven.UI;
import haven.sloth.DefSettings;

import java.util.*;

/**
 * TODO: Impossible path detection. Right now it goes into an infinite loop when it can't figure out how to path to the goal
 *       due to the goal being impossible
 */
public class APathfinder extends Pathfinder {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();

    public APathfinder(final UI ui) {
        super(ui);
    }

    private List<Coord> findpath(final Coord st, final Coord goal, final boolean allowbest, final int level) {
	//Steps to attempt
        final PriorityQueue<Node> pq = new PriorityQueue<>();
	//Steps we already tried, no reason to do them again
	final Set<Coord> ignore = new HashSet<>();
	//node -> distance
	final Map<Coord, Integer> depthmap = new HashMap<>();
	final Map<Coord, Double> distance = new HashMap<>();
	//child -> parent
	final Map<Coord, Coord> parents = new HashMap<>();
	Coord best;

	{
	    final Node stnode = new Node(st, 0);
	    best = st;
	    pq.add(stnode);
	    depthmap.put(stnode.c, 0);
	    distance.put(stnode.c, st.dist(goal));
	    parents.put(st, null);
	    ignore.add(st);
	}

	while(!pq.isEmpty()) {
	    final Node node = pq.poll();
	    final int depth = depthmap.get(node.c);

	    if(!node.c.equals(goal)) {
		for(Coord step : dirs[level]) {
		    final Coord nc = node.c.add(step);
		    //skip over ones we already did
		    if (!ignore.contains(nc)) {
			ignore.add(nc);
			if(!checkHit(nc)) {
			    final int ndepth = depth + 1;
			    final double gval = nc.dist(goal);
			    final double dist = ndepth + gval;
			    pq.add(new Node(nc, dist));
			    depthmap.put(nc, ndepth);
			    distance.put(nc, gval);
			    parents.put(nc, node.c);
			    if(gval < distance.get(best))
				best = nc;
			}
		    }
	   	 }
	    } else {
	        //Done, we hit our goal
		return collect(node.c, parents);
	    }
	}

	//never hit our goal, but we can go with our closest one
	if(allowbest) {
	    return collect(best, parents);
	}
	return null;
    }

    public ArrayList<Move> path(final Coord start, final Coord goal, final boolean allowbest) {
        return advreduce(findpath(start, goal, allowbest, DefSettings.PATHFINDINGTIER.get()-1));
    }
}
