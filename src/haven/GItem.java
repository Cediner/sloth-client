/*
 *  This file is part of the Haven & Hearth game client.
 *  Copyright (C) 2009 Fredrik Tolf <fredrik@dolda2000.com>, and
 *                     Björn Johannessen <johannessen.bjorn@gmail.com>
 *
 *  Redistribution and/or modification of this file is subject to the
 *  terms of the GNU Lesser General Public License, version 3, as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  Other parts of this source tree adhere to other copying
 *  rights. Please see the file `COPYING' in the root directory of the
 *  source tree for details.
 *
 *  A copy the GNU Lesser General Public License is distributed along
 *  with the source tree of which this file is a part in the file
 *  `doc/LPGL-3'. If it is missing for any reason, please see the Free
 *  Software Foundation's website at <http://www.fsf.org/>, or write
 *  to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
 */

package haven;

import haven.resutil.FoodInfo;
import haven.sloth.gui.item.ContentData;
import haven.sloth.io.ItemData;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class GItem extends AWidget implements ItemInfo.SpriteOwner, GSprite.Owner {
    @RName("item")
    public static class $_ implements Factory {
        public Widget create(UI ui, Object[] args) {
            int res = (Integer) args[0];
            Message sdt = (args.length > 1) ? new MessageBuf((byte[]) args[1]) : Message.nil;
            return (new GItem(ui.sess.getres(res), sdt));
        }
    }

    public interface ColorInfo {
        public Color olcol();
    }

    public interface OverlayInfo<T> {
        public T overlay();

        public void drawoverlay(GOut g, T data);
    }

    public static class InfoOverlay<T> {
        public final OverlayInfo<T> inf;
        public final T data;

        public InfoOverlay(OverlayInfo<T> inf) {
            this.inf = inf;
            this.data = inf.overlay();
        }

        public void draw(GOut g) {
            inf.drawoverlay(g, data);
        }

        public static <S> InfoOverlay<S> create(OverlayInfo<S> inf) {
            return (new InfoOverlay<S>(inf));
        }
    }

    public interface NumberInfo extends OverlayInfo<Tex> {
        public int itemnum();

        public default Color numcolor() {
            return (Color.WHITE);
        }

        public default Tex overlay() {
            return (new TexI(GItem.NumberInfo.numrender(itemnum(), numcolor())));
        }

        public default void drawoverlay(GOut g, Tex tex) {
            g.aimage(tex, g.sz, 1, 1);
        }

        public static BufferedImage numrender(int num, Color col) {
            return (Utils.outline2(Text.render(Integer.toString(num), col).img, Utils.contrast(col)));
        }
    }

    public interface MeterInfo {
        public double meter();
    }

    public static class Amount extends ItemInfo implements NumberInfo {
        private final int num;

        public Amount(Owner owner, int num) {
            super(owner);
            this.num = num;
        }

        public int itemnum() {
            return (num);
        }
    }

    private static final Text.Foundry avg_txt = new Text.Foundry(Text.sansb, 10);
    private static final Color qcol = new Color(230, 255, 255);
    public Indir<Resource> res;
    public MessageBuf sdt;
    public int meter = 0;
    public int num = -1;
    private GSprite spr;
    private ItemInfo.Raw rawinfo;
    private List<ItemInfo> info = Collections.emptyList();
    public int quality;
    public Tex q_tex;
    private WItem witem = null;
    public boolean delayediact = false;


    public GItem(Indir<Resource> res, Message sdt) {
        this.res = res;
        this.sdt = new MessageBuf(sdt);
    }

    public GItem(Indir<Resource> res) {
        this(res, Message.nil);
    }

    @Override
    protected void binded() {
        super.binded();
        if (delayediact) {
            wdgmsg("iact", Coord.o, 0);
        }
    }

    public void updateQuality(final int q) {
        this.quality = q;
        if (q > 0) {
            if (q_tex != null)
                q_tex.dispose();
            q_tex = new TexI(Utils.outline2(avg_txt.render(String.format("%d", q), qcol).img, Color.BLACK));
        }
    }

    private Random rnd = null;

    public Random mkrandoom() {
        if (rnd == null)
            rnd = new Random();
        return (rnd);
    }

    public Resource getres() {
        return (res.get());
    }

    private static final OwnerContext.ClassResolver<GItem> ctxr = new OwnerContext.ClassResolver<GItem>()
            .add(Glob.class, wdg -> wdg.ui.sess.glob)
            .add(Session.class, wdg -> wdg.ui.sess);

    public <T> T context(Class<T> cl) {
        return (ctxr.context(cl, this));
    }

    @Deprecated
    public Glob glob() {
        return (ui.sess.glob);
    }

    /**
     * Just for scripting
     */
    @SuppressWarnings("unused")
    public WItem witem() {
        return witem;
    }

    public void setWItem(final WItem item) {
        this.witem = item;
    }

    public GSprite spr() {
        GSprite spr = this.spr;
        if (spr == null) {
            try {
                spr = this.spr = GSprite.create(this, res.get(), sdt.clone());
            } catch (Loading l) {
            }
        }
        return (spr);
    }

    public void tick(double dt) {
        GSprite spr = spr();
        if (spr != null)
            spr.tick(dt);
    }

    public List<ItemInfo> info() {
        if (info == null)
            info = ItemInfo.buildinfo(this, rawinfo);
        return (info);
    }

    public <T> Optional<T> getinfo(Class<T> type) {
        try {
            for (final ItemInfo info : info()) {
                if (type.isInstance(info)) {
                    return Optional.of(type.cast(info));
                }
            }
            return Optional.empty();
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    public <T> Optional<T> getinfo(Class<T> type, List<ItemInfo> infolst) {
        try {
            for (final ItemInfo info : infolst) {
                if (type.isInstance(info)) {
                    return Optional.of(type.cast(info));
                }
            }
            return Optional.empty();
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    public <T> List<T> getinfos(Class<T> type) {
        final List<T> infos = new ArrayList<>();
        try {
            for (final ItemInfo info : info()) {
                if (type.isInstance(info)) {
                    infos.add(type.cast(info));
                }
            }
            return infos;
        } catch (Exception e) {
            return infos;
        }
    }

    public Optional<String> name() {
        final ItemInfo.Name name = getinfo(ItemInfo.Name.class).orElse(null);
        if (name != null) {
            return Optional.of(name.str.text);
        } else {
            return Optional.empty();
        }
    }

    /*******************************************************************************
     * For Scripting API only
     */
    @SuppressWarnings("unused")
    public String rnm() {
        return name().orElse("");
    }

    private static final Pattern liquid_pat = Pattern.compile("([0-9]+\\.[0-9]+) l of (.+)");
    private static final Pattern weight_pat = Pattern.compile("([0-9]+\\.[0-9]+) kg of (.+)");
    private static final Pattern seed_pat = Pattern.compile("([0-9]+) seeds of (.+)");
    private static final Pattern[] contpats = {liquid_pat, weight_pat, seed_pat};
    private static final ItemData.ContainerType[] conttypes = {ItemData.ContainerType.LIQUID, ItemData.ContainerType.WEIGHT, ItemData.ContainerType.SEED};


    public ContentData hasContents() {
        final Optional<ItemInfo.Contents> cont = getinfo(ItemInfo.Contents.class);
        if(cont.isPresent()) {
            final Optional<ItemInfo.Name.Name> contname = getinfo(ItemInfo.Name.Name.class, cont.get().sub);
            if(contname.isPresent()) {
                final Optional<String> name = name();
                if (name.isPresent()) {
                    for (int i = 0; i < contpats.length; ++i) {
                        final Matcher match = contpats[i].matcher(contname.get().str.text);
                        if (match.find()) {
                            return new ContentData(conttypes[i], match.group(2),
                                    Double.parseDouble(match.group(1)),
                                    ItemData.maxContent(name.get(), conttypes[i]));
                        }
                    }
                }
            }
        }

        return null;
    }

    public String[] getRawContents() {
        final ArrayList<String> contents = new ArrayList<>();

        for (ItemInfo.Contents cont : getinfos(ItemInfo.Contents.class)) {
            getinfo(ItemInfo.Name.Name.class, cont.sub)
                    .ifPresent((cnt) -> contents.add(cnt.str.text));
        }

        return contents.toArray(new String[0]);
    }

    public boolean isFood() {
        return getinfo(FoodInfo.class).isPresent();
    }

    /******************************************************************************/

    public Resource resource() {
        return (res.get());
    }

    public GSprite sprite() {
        if (spr == null)
            throw (new Loading("Still waiting for sprite to be constructed"));
        return (spr);
    }

    public void uimsg(String name, Object... args) {
        if (name == "num") {
            num = (Integer) args[0];
        } else if (name == "chres") {
            synchronized (this) {
                res = ui.sess.getres((Integer) args[0]);
                sdt = (args.length > 1) ? new MessageBuf((byte[]) args[1]) : MessageBuf.nil;
                spr = null;
            }
        } else if (name == "tt") {
            info = null;
            rawinfo = new ItemInfo.Raw(args);
        } else if (name == "meter") {
            meter = (int) ((Number) args[0]).doubleValue();
        }
    }
}
