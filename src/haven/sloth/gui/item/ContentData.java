package haven.sloth.gui.item;

import haven.sloth.io.ItemData;

public class ContentData {
    public final ItemData.ContainerType type;
    public final String name;
    public final double current;
    public final double max;

    public ContentData(final ItemData.ContainerType type, final String name,
                       final double size, final double max) {
        this.type = type;
        this.name = name;
        this.current = size;
        this.max = max;
    }
}
