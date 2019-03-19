package haven.sloth.script.pathfinding;

import haven.Coord;

import java.util.Objects;

public class Node implements Comparable<Node> {
    //Actual coordinate of this node
    public final Coord c;
    //Our Heuristic value
    public final double distance;

    public Node(final Coord c, final double distance) {
        this.c = c;
        this.distance = distance;
    }

    @Override
    public int hashCode() {
        return Objects.hash(c.x, c.y);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof Node && ((Node) obj).c.equals(c);
    }

    @Override
    public int compareTo(Node o) {
	return Double.compare(distance, o.distance);
    }
}
