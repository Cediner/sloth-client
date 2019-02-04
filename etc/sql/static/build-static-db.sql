--PRAGMA JOURNAL_MODE=wal;
CREATE TABLE IF NOT EXISTS type (
    type_id     INTEGER,    -- Alias for ROWID
    name_key    TEXT,       -- All Caps name of this type
    CONSTRAINT type_pk_type_id PRIMARY KEY (type_id),
    CONSTRAINT type_un_name_key UNIQUE (name_key)
);
--Our types
INSERT OR IGNORE INTO type (name_key) VALUES ('PLANT');
INSERT OR IGNORE INTO type (name_key) VALUES ('HUMAN');
INSERT OR IGNORE INTO type (name_key) VALUES ('ANIMAL');
INSERT OR IGNORE INTO type (name_key) VALUES ('VEHICLE');
INSERT OR IGNORE INTO type (name_key) VALUES ('TILE');
INSERT OR IGNORE INTO type (name_key) VALUES ('SOUND');
INSERT OR IGNORE INTO type (name_key) VALUES ('FARMING');
INSERT OR IGNORE INTO type (name_key) VALUES ('UNKNOWN');

CREATE TABLE IF NOT EXISTS object (
    object_id   INTEGER,    -- Alias for ROWID
    name        TEXT,       -- The res name of the obj
    type_id     INTEGER,    -- The type of the obj
    CONSTRAINT object_pk_object_id PRIMARY KEY (object_id),
    CONSTRAINT object_un_name UNIQUE (name),
    CONSTRAINT object_fk_type_id FOREIGN KEY (type_id) REFERENCES type(type_id) ON DELETE CASCADE
);
-- Specific objects we care about
-- plants
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/flax', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/barley', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/carrot', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/poppy', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/hemp', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/pipeweed', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/beet', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/hops', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/peas', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/yellowonion', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/pumpkin', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/pepper', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/wine', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
-- misc stuff
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/trough', (SELECT type_id FROM type WHERE name_key = 'FARMING'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/beehive', (SELECT type_id FROM type WHERE name_key = 'FARMING'));
-- humans
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/borka/body', (SELECT type_id FROM type WHERE name_key = 'HUMAN'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/borka/wisp', (SELECT type_id FROM type WHERE name_key = 'HUMAN'));
-- vehicles
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/cart'			, (SELECT type_id FROM type WHERE name_key = 'VEHICLE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/catapult'		, (SELECT type_id FROM type WHERE name_key = 'VEHICLE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/knarr'			, (SELECT type_id FROM type WHERE name_key = 'VEHICLE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/plow'			, (SELECT type_id FROM type WHERE name_key = 'VEHICLE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/raft'			, (SELECT type_id FROM type WHERE name_key = 'VEHICLE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/rowboat'		, (SELECT type_id FROM type WHERE name_key = 'VEHICLE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/wagon'			, (SELECT type_id FROM type WHERE name_key = 'VEHICLE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/wheelbarrow'	, (SELECT type_id FROM type WHERE name_key = 'VEHICLE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/wreckingball'	, (SELECT type_id FROM type WHERE name_key = 'VEHICLE'));
-- animals
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/badger/badger', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/walrus/walrus', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/bear/bear', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/bat/bat', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/boar/boar', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/mammoth/mammoth', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/troll/troll', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/lynx/lynx', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/wolf/wolf', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/fox/fox', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/rabbit/rabbit', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/reddeer/reddeer', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/dryad/dryad', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/ants/ants', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/cattle/bull', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/cattle/cattle', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/cattle/cow', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/cattle/aurochs', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/chicken/chick', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/chicken/chicken', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/chicken/hen', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/chicken/rooster', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/hedgehog/hedgehog', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/horse/horse', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/horse/mare', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/horse/stallion', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/ladybug/ladybug', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/moose/moose', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/rat/rat', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/sheep/sheep', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/sheep/ram', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/sheep/mouflon', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/silkmoth/silkmoth', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/squirrel/squirrel', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/toad/toad', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/frog/frog', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/dragonfly/dragonfly', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/wolverine/wolverine', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/adder/adder', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/beaver/beaver', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
-- vehicles
-- tiles
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/beach', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/beechgrove', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/bluesod', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/boards', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/bog', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/bogwater', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/cave', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/cloudrange', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/deep', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/dirt', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/dryflat', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/fen', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/fenwater', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/field', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/flowermeadow', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/grass', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/greensward', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/hardsteppe', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/heath', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/highground', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/leaf', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/leafpatch', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/lichenwold', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/lushfield', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/mine', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/moor', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/mossbrush', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/mountain', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/nil', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/oakwilds', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/acrebrick', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/argentite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/ballbrick', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/basalt', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/blackcoal', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/cassiterite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/catgold', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/chalcopyrite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/cinnabar', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/dolomite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/feldspar', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/flint', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/galena', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/gneiss', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/granite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/hematite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/ilmenite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/limestone', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/limonite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/magnetite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/malachite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/marble', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/nagyagite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/petzite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/porphyry', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/quartz', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/sandstone', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/schist', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/slag', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/paving/sylvanite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/pinebarren', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/redplain', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/ridges/cavein', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/ridges/caveout', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/ridges/soil', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/argentite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/basalt', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/blackcoal', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/cassiterite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/chalcopyrite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/cinnabar', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/dolomite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/feldspar', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/flint', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/galena', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/gneiss', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/granite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/hematite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/ilmenite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/limestone', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/limonite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/magnetite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/malachite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/marble', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/petzite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/porphyry', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/quartz', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/sandstone', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/schist', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rocks/sylvanite', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/rootbosk', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/sandcliff', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/seabed', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/shadycopse', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/skycube', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/snow', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/sombrebramble', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/spave', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/swamp', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/swampwater', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/timberland', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/wald', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/water', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/wildturf', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/tiles/wn', (SELECT type_id FROM type WHERE name_key = 'TILE'));
-- sounds
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/blowing', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/bone', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/breakwood', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/balders', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/build', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/chip', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/choppan', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/clank', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/clonk', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/creak2', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/creakdoor', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/drinkan', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/exp', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/heavydoor', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/lvlup', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/fanfar', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/farman', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/hammer', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/heathbird', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/jump', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/land', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/squeak', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/swoosh', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/match', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/meat', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/metalhinge', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/metalhit', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/mineout', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/plop', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/plums', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/plums-big', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/ropecreak', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/runningwater', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/treefall', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/thud', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/wading', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/twang', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/borka/butcher', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/borka/leafrustle', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/borka/shoveldig', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/borka/sowing', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/borka/cc-scream', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/borka/bitedust', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/fight/antspit', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/fight/armorcrash', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/fight/arm-soak1', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/fight/arm-soak2', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/fight/you-lose', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/fight/you-win', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/fx/flame', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/fx/metal', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/fx/burnhand', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/fx/tar', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/inst/drum', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/inst/fiddle', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/inst/flute', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/inst/lute', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/items/pickaxe', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/items/stretch', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/terobjs/items/die', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/terobjs/arch/door', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/terobjs/anvil', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/terobjs/cauldron', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/terobjs/grinder', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/terobjs/plow', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/terobjs/quern', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/terobjs/rustygate', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/terobjs/swheel', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/terobjs/thud-wood', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/terobjs/woodcrash', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/terobjs/woodcrash2', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('sfx/terobjs/woodspin', (SELECT type_id FROM type WHERE name_key = 'SOUND'));

CREATE TABLE IF NOT EXISTS growth (
    object_id   INTEGER,    -- Objects that can grow and have stages
    final_stage INTEGER,    -- The final growth stage when it can be harvested
    CONSTRAINT plant_pk_object_id PRIMARY KEY (object_id),
    CONSTRAINT plant_fk_object_id FOREIGN KEY (object_id) REFERENCES object(object_id) ON DELETE CASCADE
);
--Mainly plants
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/flax')          , 3);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/barley')        , 3);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/carrot')        , 3);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/poppy')         , 4);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/hemp')          , 3);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/pipeweed')      , 4);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/beet')          , 3);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/hops')          , 6);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/peas')          , 4);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/yellowonion')   , 3);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/pumpkin')       , 4);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/pepper')        , 6);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/wine')          , 6);

CREATE TABLE IF NOT EXISTS dangerous (
    object_id   INTEGER,    -- Objects that are dangerous to the player
    CONSTRAINT dangerous_pk_object_id PRIMARY KEY (object_id),
    CONSTRAINT dangerous_fk_object_id FOREIGN KEY (object_id) REFERENCES object(object_id) ON DELETE CASCADE
);
-- animals
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/badger/badger'	));
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/bear/bear'		));
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/bat/bat'		));
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/boar/boar'		));
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/mammoth/mammoth'));
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/troll/troll'	));
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/lynx/lynx'		));
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/wolf/wolf'		));
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/wolverine/wolverine'));
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/adder/adder'	));
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/beaver/beaver'	));

CREATE TABLE IF NOT EXISTS range (
    object_id   INTEGER,    -- Objects that have some kind of radius for a reason
    radius      INTEGER,    -- The circular range in tiles
    CONSTRAINT dangerous_pk_object_id PRIMARY KEY (object_id),
    CONSTRAINT dangerous_fk_object_id FOREIGN KEY (object_id) REFERENCES object(object_id) ON DELETE CASCADE
);
-- objs with range
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/walrus/walrus'	), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/badger/badger'	), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/bear/bear'		), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/bat/bat'		), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/boar/boar'		), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/mammoth/mammoth'), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/troll/troll'	), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/lynx/lynx'		), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/wolf/wolf'		), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/trough'         ), 18);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/beehive'        ), 13);
-- While true, these have a built in way to display their Radius and there's no point to duplicate that
--INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/minesupport'    ), 9);
--INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/column'         ), 11);

CREATE TABLE IF NOT EXISTS move (
    object_id   INTEGER,    -- Objects that can move
    CONSTRAINT move_pk_object_id PRIMARY KEY (object_id),
    CONSTRAINT move_fk_object_id FOREIGN KEY (object_id) REFERENCES object(object_id) ON DELETE CASCADE
);
-- all humans
INSERT OR IGNORE INTO move SELECT object_id FROM object WHERE type_id = (SELECT type_id FROM type WHERE name_key = 'HUMAN');
-- some vehicles, mainly boats
INSERT OR IGNORE INTO move VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/vehicle/knarr'));
INSERT OR IGNORE INTO move VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/vehicle/rowboat'));
INSERT OR IGNORE INTO move VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/vehicle/raft'));
INSERT OR IGNORE INTO move VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/vehicle/wagon'));
-- all the animals
INSERT OR IGNORE INTO move SELECT object_id FROM object WHERE type_id = (SELECT type_id FROM type WHERE name_key = 'ANIMAL');