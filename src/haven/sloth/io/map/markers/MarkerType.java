package haven.sloth.io.map.markers;

public enum MarkerType {
    PLAYER_MARKER(0),
    SERVER_MARKER(1),
    SLOTH_MARKER(2),
    REALM_MARKER(3),
    LINKED_MARKER(4);

    public final byte id;

    MarkerType(final int id) {
        this.id = (byte) id;
    }
}
