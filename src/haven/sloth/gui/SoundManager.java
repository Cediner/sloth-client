package haven.sloth.gui;

import haven.*;
import haven.sloth.gob.Alerted;
import haven.sloth.gob.Hidden;
import haven.sloth.util.ObservableMapListener;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class SoundManager extends Window implements ObservableMapListener<String, Resource.Named> {
    private Map<String, Resource.Named> map = new TreeMap<>();
    private List<String> keys = new ArrayList<>();
    private final Listbox<Resource.Named> sounds;
    private final Listbox<String> objs;

    public SoundManager() {
        super(Coord.z, "Sound Manager");
	Coord c = new Coord(0, 0);
	c.x += add(objs = new Listbox<String>(200, 20, 20) {
	    @Override
	    protected String listitem(int i) {
		return keys.get(i);
	    }

	    @Override
	    protected int listitems() {
		return keys.size();
	    }

	    @Override
	    protected void drawitem(GOut g, String item, int i) {
		g.text(item, new Coord(5, 1));
	    }

	    @Override
	    public void change(String item) {
	        if(item != null) {
		    sounds.sel = map.get(item);
		}
	        super.change(item);
	    }
	}, c.copy()).sz.x + 5;
	c.y += add(sounds = new Listbox<Resource.Named>(200, 20, 20) {
	    @Override
	    protected Resource.Named listitem(int i) {
		return Alerted.sounds.get(i);
	    }

	    @Override
	    protected int listitems() {
		return Alerted.sounds.size();
	    }

	    @Override
	    protected void drawitem(GOut g, Resource.Named item, int i) {
		g.text(item.name, new Coord(5, 1));
	    }
	}, c.copy()).sz.y + 5;
	{
	    Coord bc = c.copy();
	    bc.y += add(new Button(200, "Select", this::select), bc.copy()).sz.y + 5;
	    add(new Button(200, "Preview", this::preview), bc.copy());
	}
	{
	    c.x = 0;
	    add(new Button(200, "Remove Sound Alert", this::removeAlert), c.copy());
	}
	pack();
	Alerted.listen(this);
    }

    private void removeAlert() {
	if (objs.sel != null) {
	    Alerted.remove(objs.sel);
	}
    }

    private void select() {
	if (objs.sel != null) {
	    if (sounds.sel != null) {
		Alerted.add(objs.sel, sounds.sel);
	    }
	}
    }

    private void preview() {
	if(sounds.sel != null) {
	    Audio.play(sounds.sel);
	}
    }

    @Override
    public void close() {
	Alerted.unlisten(this);
	ui.destroy(this);
    }

    @Override
    public void init(Map<String, Resource.Named> base) {
	map.putAll(base);
	keys.addAll(map.keySet());
    }

    public void put(String key, Resource.Named val) {
        if(!map.containsKey(key)) {
	    map.put(key, val);
	    keys.add(key);
	} else {
            //only an update
            map.put(key, val);
	}
    }

    public void remove(String key) {
        map.remove(key);
        keys.remove(key);
    }
}
