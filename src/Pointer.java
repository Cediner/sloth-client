import haven.*;

public class Pointer extends Widget {
    public static final States.ColState col = new States.ColState(241, 227, 157, 255);
    public Indir<Resource> icon;
    private Tex licon;
    public Coord2d tc;
    public Coord lc;
    public long gobid = -1L;

    public Pointer(Indir<Resource> paramIndir)
    {
	super(Coord.z);
	this.icon = paramIndir;
    }

    public static Widget mkwidget(UI paramUI, Object... paramVarArgs)
    {
	int i = ((Integer)paramVarArgs[0]);
	return new Pointer(i < 0 ? null : paramUI.sess.getres(i));
    }

    public void presize()
    {
	resize(this.parent.sz);
    }

    protected void added()
    {
	presize();
	super.added();
    }

    private int signum(int paramInt)
    {
	if (paramInt < 0) {
	    return -1;
	}
	if (paramInt > 0) {
	    return 1;
	}
	return 0;
    }

    private void drawarrow(GOut paramGOut, Coord paramCoord)
    {
	Coord localCoord1 = this.sz.div(2);
	paramCoord = paramCoord.sub(localCoord1);
	if (paramCoord.equals(Coord.z)) {
	    paramCoord = new Coord(1, 1);
	}
	double d = Coord.z.dist(paramCoord);
	Coord localCoord2 = paramCoord.mul((d - 25.0D) / d);
	float f = localCoord1.y / localCoord1.x;
	if ((Math.abs(localCoord2.x) > localCoord1.x) || (Math.abs(localCoord2.y) > localCoord1.y)) {
	    if (Math.abs(localCoord2.x) * f < Math.abs(localCoord2.y)) {
		localCoord2 = new Coord(localCoord2.x * localCoord1.y / localCoord2.y, localCoord1.y).mul(signum(localCoord2.y));
	    } else {
		localCoord2 = new Coord(localCoord1.x, localCoord2.y * localCoord1.x / localCoord2.x).mul(signum(localCoord2.x));
	    }
	}
	Coord localCoord3 = localCoord2.sub(paramCoord).norm(30.0D);
	localCoord2 = localCoord2.add(localCoord1);

	BGL localBGL = paramGOut.gl;
	paramGOut.state2d();
	paramGOut.state(col);
	paramGOut.apply();
	localBGL.glEnable(2881);
	localBGL.glBegin(4);
	paramGOut.vertex(localCoord2);
	paramGOut.vertex(localCoord2.add(localCoord3).add(-localCoord3.y / 3, localCoord3.x / 3));
	paramGOut.vertex(localCoord2.add(localCoord3).add(localCoord3.y / 3, -localCoord3.x / 3));
	localBGL.glEnd();
	localBGL.glDisable(2881);
	if (this.icon != null) {
	    try
	    {
		if (this.licon == null) {
		    this.licon = ((this.icon.get()).layer(Resource.imgc)).tex();
		}
		paramGOut.aimage(this.licon, localCoord2.add(localCoord3), 0.5D, 0.5D);
	    }
	    catch (Loading localLoading) {}
	}
	this.lc = localCoord2.add(localCoord3);
    }

    public void draw(GOut paramGOut)
    {
	this.lc = null;
	if (this.tc == null) {
	    return;
	}
	Gob localGob = this.gobid < 0L ? null : this.ui.sess.glob.oc.getgob(this.gobid);
	Coord3f localCoord3f;
	if (localGob != null) {
	    try
	    {
		localCoord3f = (getparent(GameUI.class)).map.screenxf(localGob.getc());
	    }
	    catch (Loading localLoading)
	    {
		return;
	    }
	} else {
	    localCoord3f = (getparent(GameUI.class)).map.screenxf(this.tc);
	}
	if (localCoord3f != null) {
	    drawarrow(paramGOut, new Coord(localCoord3f));
	}
    }

    public void udpate(Coord2d paramCoord2d, long paramLong)
    {
	this.tc = paramCoord2d;
	this.gobid = paramLong;
    }

    public void uimsg(String paramString, Object... paramVarArgs)
    {
	if (paramString.equals("upd"))
	{
	    if (paramVarArgs[0] == null) {
		this.tc = null;
	    } else {
		this.tc = ((Coord)paramVarArgs[0]).mul(OCache.posres);
	    }
	    if (paramVarArgs[1] == null) {
		this.gobid = -1L;
	    } else {
		this.gobid = ((Integer)paramVarArgs[1]);
	    }
	}
	else if (paramString.equals("icon"))
	{
	    int i = ((Integer)paramVarArgs[0]);
	    this.icon = i < 0 ? null : this.ui.sess.getres(i);
	    this.licon = null;
	}
	else
	{
	    super.uimsg(paramString, paramVarArgs);
	}
    }

    private Text.Line tt = null;
    private int dist;
    public Object tooltip(Coord paramCoord, Widget paramWidget)
    {
	if ((this.lc != null) && (this.lc.dist(paramCoord) < 20.0D)) {
	    if(tooltip instanceof Text.Line) {
	        final Gob me = ui.sess.glob.oc.getgob(ui.gui.map.plgob);
	        final int cdist = (int)(Math.ceil(me.rc.dist(tc)/11.0));
	        if(cdist != dist) {
	            dist = cdist;
	            final String extra;
	            if(dist >= 1000) {
	                extra = " - May be further than the client can see";
		    } else {
	                extra = "";
		    }
	            if(tt != null && tt.tex() != null)
	                tt.tex().dispose();
		    return tt = Text.render(((Text.Line) this.tooltip).text + " - Distance: " + dist + extra);
		} else {
		    return tt;
		}
	    } else {
		return this.tooltip;
	    }
	}
	return null;
    }
}
