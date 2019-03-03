package haven.sloth.gui;

import haven.*;
import haven.Button;
import haven.Window;

import java.awt.*;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

public class MapMarkerWnd extends Window {
    private final static Predicate<MapFile.Marker> pmarkers = (m -> m instanceof MapFile.PMarker);
    private final static Predicate<MapFile.Marker> smarkers = (m -> m instanceof MapFile.SMarker || m instanceof MapFile.SlothMarker);
    private final static Comparator<MapFile.Marker> namecmp = Comparator.comparing(MapFile.Marker::name);
    private Predicate<MapFile.Marker> mflt;
    private List<MapFile.Marker> markers = Collections.emptyList();
    private int markerseq = -1;
    private Comparator<MapFile.Marker> mcmp = namecmp;
    private final MapWnd map;
    public final MarkerList list;
    private TextEntry namesel;
    private BuddyWnd.GroupSelector colsel;
    private Button mremove;

    public MapMarkerWnd(final MapWnd map) {
        super(Coord.z, "Markers", "Markers");
    	this.map = map;
    	mflt = pmarkers;
	list = add(new MarkerList(200, 20));
	resize(list.sz.add(0, 120));
	add(new Button(95, "Placed", () -> {
	    mflt = pmarkers;
	    markerseq = -1;
	}), list.c.add(0, list.sz.y + 10));
	add(new Button(95, "Natural", () -> {
	    mflt = smarkers;
	    markerseq = -1;
	}), list.c.add(list.sz.x - 95, list.sz.y + 10));
	pack();
    }

    @Override
    public void close() {
	hide();
    }

    @Override
    public void tick(double dt) {
	super.tick(dt);
	if(visible && (markerseq != map.view.file.markerseq)) {
	    if(map.view.file.lock.readLock().tryLock()) {
		try {
		    this.markers = map.view.file.markers.stream().filter(mflt).sorted(mcmp).collect(java.util.stream.Collectors.toList());
		    list.display();
		} finally {
		    map.view.file.lock.readLock().unlock();
		}
	    }
	}
    }

    public static final Color every = new Color(255, 255, 255, 16), other = new Color(255, 255, 255, 32), found = new Color(255, 255, 0, 32);
    public class MarkerList extends Searchbox<MapFile.Marker> {
	private final Text.Foundry fnd = CharWnd.attrf;

	public MapFile.Marker listitem(int idx) {return(markers.get(idx));}
	public int listitems() {return(markers.size());}
	public boolean searchmatch(int idx, String txt) {
	    return markers.get(idx).nm.toLowerCase().contains(txt.toLowerCase());
	}

	private MarkerList(int w, int n) {
	    super(w, n, 20);
	}

	private Function<String, Text> names = new CachedFunction<>(500, fnd::render);
	protected void drawbg(GOut g) {}
	public void drawitem(GOut g, MapFile.Marker mark, int idx) {
	    if(soughtitem(idx)) {
		g.chcolor(found);
		g.frect(Coord.z, g.sz);
	    }
	    g.chcolor(((idx % 2) == 0)?every:other);
	    g.frect(Coord.z, g.sz);
	    if(mark instanceof MapFile.PMarker)
		g.chcolor(((MapFile.PMarker)mark).color);
	    else
		g.chcolor();
	    g.aimage(names.apply(mark.nm).tex(), new Coord(5, itemh / 2), 0, 0.5);
	}

	public void change(MapFile.Marker mark) {
	    change2(mark);
	    if(mark != null)
		map.view.center(new MapFileWidget.SpecLocator(mark.seg, mark.tc));
	}

	public void change2(MapFile.Marker mark) {
	    this.sel = mark;

	    if(namesel != null) {
		ui.destroy(namesel);
		namesel = null;
		if(colsel != null) {
		    ui.destroy(colsel);
		    colsel = null;
		    ui.destroy(mremove);
		    mremove = null;
		}
		MapMarkerWnd.this.pack();
	    }

	    if(mark != null) {
	        if(mark instanceof MapFile.PMarker) {
		    mflt = pmarkers;
		    markerseq = -1;
		} else {
		    mflt = smarkers;
		    markerseq = -1;
		}

		if(namesel == null) {
		    namesel = MapMarkerWnd.this.add(new TextEntry(200, "") {
			{dshow = true;}
			public void activate(String text) {
			    mark.nm = text;
			    map.view.file.update(mark);
			    commit();
			    change2(null);
			}
		    }, new Coord(0, MapMarkerWnd.this.csz.y));
		}
		namesel.settext(mark.nm);
		namesel.buf.point = mark.nm.length();
		namesel.commit();
		if(mark instanceof MapFile.PMarker) {
		    MapFile.PMarker pm = (MapFile.PMarker)mark;
		    colsel = MapMarkerWnd.this.add(new BuddyWnd.GroupSelector(0) {
			public void changed(int group) {
			    this.group = group;
			    pm.color = BuddyWnd.gc[group];
			    map.view.file.update(mark);
			}
		    }, namesel.c.add(0, namesel.sz.y + 10));
		    if((colsel.group = Utils.index(BuddyWnd.gc, pm.color)) < 0)
			colsel.group = 0;
		    mremove = MapMarkerWnd.this.add(new Button(200, "Remove", false) {
			public void click() {
			    map.view.file.remove(mark);
			    change2(null);
			}
		    }, colsel.c.add(0, colsel.sz.y + 10));
		}
		MapMarkerWnd.this.pack();
	    }
	    MapMarkerWnd.this.show();
	}
    }
}
