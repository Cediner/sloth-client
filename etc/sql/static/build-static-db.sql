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
INSERT OR IGNORE INTO type (name_key) VALUES ('TAMEDANIMAL');
INSERT OR IGNORE INTO type (name_key) VALUES ('SMALLANIMAL');
INSERT OR IGNORE INTO type (name_key) VALUES ('WATERVEHICLE');
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
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/redonion', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/turnip', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/cucumber', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/millet', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/leek', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/plants/lettuce', (SELECT type_id FROM type WHERE name_key = 'PLANT'));
-- misc stuff
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/trough', (SELECT type_id FROM type WHERE name_key = 'FARMING'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/beehive', (SELECT type_id FROM type WHERE name_key = 'FARMING'));
-- humans
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/borka/body', (SELECT type_id FROM type WHERE name_key = 'HUMAN'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/borka/wisp', (SELECT type_id FROM type WHERE name_key = 'HUMAN'));
-- water vehicles
INSERT OR REPLACE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/knarr'			, (SELECT type_id FROM type WHERE name_key = 'WATERVEHICLE'));
INSERT OR REPLACE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/rowboat'		, (SELECT type_id FROM type WHERE name_key = 'WATERVEHICLE'));
INSERT OR REPLACE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/coracle'		, (SELECT type_id FROM type WHERE name_key = 'WATERVEHICLE'));
INSERT OR REPLACE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/dugout'		, (SELECT type_id FROM type WHERE name_key = 'WATERVEHICLE'));
-- vehicles
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/cart'			, (SELECT type_id FROM type WHERE name_key = 'VEHICLE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/catapult'		, (SELECT type_id FROM type WHERE name_key = 'VEHICLE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/plow'			, (SELECT type_id FROM type WHERE name_key = 'VEHICLE'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/terobjs/vehicle/raft'			, (SELECT type_id FROM type WHERE name_key = 'VEHICLE'));
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
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/reddeer/reddeer', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/dryad/dryad', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/ants/ants', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/cattle/aurochs', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/moose/moose', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/sheep/mouflon', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/wolverine/wolverine', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/adder/adder', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/beaver/beaver', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/swan/swan', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/caveangler/caveangler', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/otter/otter', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/reindeer/reindeer', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/ooze/greenooze', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/goldeneagle/goldeneagle', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/boreworm/boreworm', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/greyseal/greyseal', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/spermwhale/spermwhale', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/wildbees/beeswarm', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/goat/wildgoat', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/cavelouse/cavelouse', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/rat/caverat', (SELECT type_id FROM type WHERE name_key = 'ANIMAL'));

INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/sheep/sheep', (SELECT type_id FROM type WHERE name_key = 'TAMEDANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/cattle/bull', (SELECT type_id FROM type WHERE name_key = 'TAMEDANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/cattle/cattle', (SELECT type_id FROM type WHERE name_key = 'TAMEDANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/cattle/cow', (SELECT type_id FROM type WHERE name_key = 'TAMEDANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/sheep/ram', (SELECT type_id FROM type WHERE name_key = 'TAMEDANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/goat/billy', (SELECT type_id FROM type WHERE name_key = 'TAMEDANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/goat/nanny', (SELECT type_id FROM type WHERE name_key = 'TAMEDANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/horse/horse', (SELECT type_id FROM type WHERE name_key = 'TAMEDANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/horse/mare', (SELECT type_id FROM type WHERE name_key = 'TAMEDANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/horse/stallion', (SELECT type_id FROM type WHERE name_key = 'TAMEDANIMAL'));

INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/crab/crab', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/rabbit/rabbit', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/chicken/chick', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/chicken/chicken', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/chicken/hen', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/chicken/rooster', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/hedgehog/hedgehog', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/ladybug/ladybug', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/rat/rat', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/squirrel/squirrel', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/toad/toad', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/frog/frog', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/silkmoth/silkmoth', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('gfx/kritter/dragonfly/dragonfly', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/grasshopper/grasshopper', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/forestlizard/forestlizard', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/waterstrider/waterstrider', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/irrbloss/irrbloss', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/forestsnail/forestsnail', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/stagbeetle/stagbeetle', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/firefly/firefly', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/jellyfish/jellyfish', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/cavecentipede/cavecentipede', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/cavemoth/cavemoth', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/kritter/mallard/mallard', (SELECT type_id FROM type WHERE name_key = 'SMALLANIMAL'));
-- vehicles
-- tiles
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/tiles/beach', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/tiles/beechgrove', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/tiles/bluesod', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/tiles/boards', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/tiles/bog', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/tiles/bogwater', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/tiles/cave', (SELECT type_id FROM type WHERE name_key = 'TILE'));
INSERT OR IGNORE INTO object (name, type_id)
VALUES ('gfx/tiles/cloudrange', (SELECT type_id FROM type WHERE name_key = 'TILE'));
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
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/howl', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/timer', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/AOE1_House', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/AOE1_PriestConvert2', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/AOE1_Select1', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/AOE2_AttackWarning', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/AOE2_Chat', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/AOE2_Dock', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/AOE2_EnemyConvertSuccess', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/AOE2_Harp', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/AOE2_MilitaryCreation', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/AOE2_Relic', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/AOE2_TownBell', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/AOE2_Tribute', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/AOE2_VillagerCreation', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/BounceOdd', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/MGS3_Alert', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/MK2_FinishHim', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Pinging', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/SM64_Boo', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/SMB2_Shrink', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/SMB3_1-Up', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/SMB3_Coin', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/SMB3_Jump', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/SMB3_Mushroom_Appears', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/SMB3_Pause', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/SMB3_Power-Up', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/SMB3_Tail', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/SMB3_Vine', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Sonic2_Jump', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Sonic2_Ring', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Sonic2_RingsLost', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_LOZ_Recorder', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_LOZ_Secret', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_OOT_Fanfare_HeartContainer', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_OOT_Fanfare_Item', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_OOT_Fanfare_SmallItem', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_OOT_Get_SmallItem', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_OOT_GoldSkulltula_Token', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_OOT_Navi_Hey', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_OOT_Navi_In', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_OOT_Navi_Listen', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_OOT_Navi_Out', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_OOT_Navi_WatchOut', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_PH_Fanfare_GetNothing', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_PH_Fanfare_GetRupoor', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_SS_Groose_Doh', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_SS_Groose_Ohh', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_SS_Heart', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_SS_Rupee', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_SS_Rupee_Blue', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_SS_StaminaFruit', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_SS_StaminaFruit_Regrow', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_ST_Fanfare_GetSmallItem', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_TP_Secret', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_WW_Get_Item', (SELECT type_id FROM type WHERE name_key = 'SOUND'));
INSERT OR IGNORE INTO object (name, type_id) VALUES('custom/sfx/omni/Z_WW_Get_Rupee_Blue', (SELECT type_id FROM type WHERE name_key = 'SOUND'));

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
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/redonion')      , 3);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/turnip')        , 3);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/cucumber')      , 4);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/leek')          , 4);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/lettuce')       , 4);
INSERT OR IGNORE INTO growth VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/plants/millet')        , 3);

CREATE TABLE IF NOT EXISTS dangerous (
    object_id   INTEGER,    -- Objects that are dangerous to the player
    CONSTRAINT dangerous_pk_object_id PRIMARY KEY (object_id),
    CONSTRAINT dangerous_fk_object_id FOREIGN KEY (object_id) REFERENCES object(object_id) ON DELETE CASCADE
);
-- animals
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/badger/badger'	        ));
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/bear/bear'		        ));
--INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/bat/bat'		));
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/boar/boar'		        ));
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/mammoth/mammoth'        ));
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/troll/troll'	        ));
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/lynx/lynx'		        ));
INSERT OR IGNORE INTO dangerous VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/wolf/wolf'		        ));
INSERT OR IGNORE INTO dangerous
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/wolverine/wolverine'));
INSERT OR IGNORE INTO dangerous
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/adder/adder'));
INSERT OR IGNORE INTO dangerous
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/beaver/beaver'));
INSERT OR IGNORE INTO dangerous
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/walrus/walrus'));
INSERT OR IGNORE INTO dangerous
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/caveangler/caveangler'));
INSERT OR IGNORE INTO dangerous
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/ooze/greenooze'));
INSERT OR IGNORE INTO dangerous
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/goldeneagle/goldeneagle'));
INSERT OR IGNORE INTO dangerous
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/boreworm/boreworm'));
INSERT OR IGNORE INTO dangerous
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/spermwhale/spermwhale'));
INSERT OR IGNORE INTO dangerous
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/cavelouse/cavelouse'));
INSERT OR IGNORE INTO dangerous
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/goat/wildgoat'));
INSERT OR IGNORE INTO dangerous
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/rat/caverat'));

CREATE TABLE IF NOT EXISTS range
(
    object_id INTEGER, -- Objects that have some kind of radius for a reason
    radius    INTEGER, -- The circular range in tiles
    CONSTRAINT dangerous_pk_object_id PRIMARY KEY (object_id),
    CONSTRAINT dangerous_fk_object_id FOREIGN KEY (object_id) REFERENCES object (object_id) ON DELETE CASCADE
);
-- objs with range
INSERT OR IGNORE INTO range
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/walrus/walrus'), 10);
INSERT OR IGNORE INTO range
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/badger/badger'), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/bear/bear'		), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/bat/bat'		), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/boar/boar'		), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/mammoth/mammoth'), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/troll/troll'	), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/lynx/lynx'		), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/wolf/wolf'		), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/wolverine/wolverine'), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/adder/adder'	), 10);
INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/beaver/beaver'	), 10);
INSERT OR IGNORE INTO range
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/trough'), 18);
INSERT OR IGNORE INTO range
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/beehive'), 13);
INSERT OR IGNORE INTO range
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/cavelouse/cavelouse'), 10);
INSERT OR IGNORE INTO range
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/goat/wildgoat'), 10);
INSERT OR IGNORE INTO range
VALUES ((SELECT object_id FROM object WHERE name = 'gfx/kritter/rat/caverat'), 10);
-- While true, these have a built in way to display their Radius and there's no point to duplicate that
-- INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/minesupport'    ), 9);
-- INSERT OR IGNORE INTO range VALUES ((SELECT object_id FROM object WHERE name = 'gfx/terobjs/column'         ), 11);

CREATE TABLE IF NOT EXISTS move
(
    object_id INTEGER, -- Objects that can move
    CONSTRAINT move_pk_object_id PRIMARY KEY (object_id),
    CONSTRAINT move_fk_object_id FOREIGN KEY (object_id) REFERENCES object (object_id) ON DELETE CASCADE
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



------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
-- Item details
--The main type of an item, not the best mapping since things can be both a tool and weapon for instance, not a big deal
--tho
CREATE TABLE IF NOT EXISTS item_type (
    item_type_id     INTEGER,    -- Alias for ROWID
    name_key         TEXT,       -- All Caps name of this type
    CONSTRAINT item_type_pk_item_type_id PRIMARY KEY (item_type_id),
    CONSTRAINT item_type_un_name_key UNIQUE (name_key)
);

CREATE TABLE IF NOT EXISTS item (
    item_id      INTEGER,   --Alias for RowID
    item_type_id INTEGER,   --Item type
    name_key     TEXT,      --Item 'Name' ItemInfo, all uppercase
    CONSTRAINT item_pk_item_id PRIMARY KEY (item_id),
    CONSTRAINT item_un_name UNIQUE (name_key)
);

-- Items that allow liquids/seeds, etc
CREATE TABLE IF NOT EXISTS item_contents (
    item_id      INTEGER,   --Alias for RowID
    liquid_max   REAL,
    weight_max   REAL,
    seed_max     REAL,
    CONSTRAINT item_contents_pk_item_id PRIMARY KEY (item_id)
);

CREATE TABLE IF NOT EXISTS item_equipable (
    item_id INTEGER, -- Alias for ROWID
    CONSTRAINT item_equipable_pk_item_id PRIMARY KEY (item_id)
);

INSERT OR IGNORE INTO item_type (name_key) VALUES ("EQUIPMENT");
INSERT OR IGNORE INTO item_type (name_key) VALUES ("CONTAINER");
INSERT OR IGNORE INTO item_type (name_key) VALUES ("ARMOR");
INSERT OR IGNORE INTO item_type (name_key) VALUES ("WEAPON");
INSERT OR IGNORE INTO item_type (name_key) VALUES ("EQUIPMENT"); --Misc Equipment, gilding items, just for show, etc
INSERT OR IGNORE INTO item_type (name_key) VALUES ("TOOL");
INSERT OR IGNORE INTO item_type (name_key) VALUES ("ARROW");
INSERT OR IGNORE INTO item_type (name_key) VALUES ("FISHING");

INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'WEAPON'), "BATTLEAXE OF THE TWELFTH BAY");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'WEAPON'), "BOAR SPEAR");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'WEAPON'), "BRONZE SWORD");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'WEAPON'), "CUTBLADE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'WEAPON'), "FYRDSMAN'S SWORD");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'WEAPON'), "HIRDSMAN'S SWORD");

INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'CONTAINER'), 'WATERSKIN');
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'CONTAINER'), 'WATERFLASK');
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'CONTAINER'), 'BIRCHBARK KUKSA');
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'CONTAINER'), 'BUCKET');
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "ADDER CROWN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "AIR MARSHAL'S CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "ANT CROWN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "ANT HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "ANTENNA HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "ARMORED STRIDERS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "ASHEN ROBES");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "ASHTRAY HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "ASTROLOGER'S CONE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "AUTUMN BOUNTY");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "BADGER HIDE VEST");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BAGPIPE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BANDIT'S MASK");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BARBERSHOP BOATER");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BAT HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "BAT WING");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BATTLE STANDARD");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BEAR CAPE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BEAR COAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BEAR TOOTH TALISMAN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BEARSKIN CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BEAST RING");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BEAVER CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BEAVER CROWN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "BEAVER WRIST GUARDS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BEE HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BEERHAT");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BICYCLE HELMET");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BIRCHBARK BACKPACK");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BISHOP'S MITRE");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BLUEBELL HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BLUEBERRY HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BLUEBUCKET HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "BOAR TUSK HELMET");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BOBBY HELMET");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARROW'), "BONE ARROW");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "BONE GREAVES");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'TOOL'), "BONESAW");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "BOREWORM MASK");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BOSSA NOVA RED");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BOUQUET OF FLOWERS");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BRAIN IN A JAR");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "BRONZE HELM");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "BRONZE PLATE");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BROWN PAPER BAG");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BULL PIPE");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "BUNNY SLIPPERS");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'FISHING'), "BUSHCRAFT FISHINGPOLE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'WEAPON'), "BUTCHER'S CLEAVER");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CAMPAIGN CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CANDLE CROWN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CAP OF THE INTERNATIONAL BRIGADES");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CAT HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CAVE CORAL RING");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CENTURION'S HELM");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "CHAINMAIL SHIRT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CHE'S BERET");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CHEF'S HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CHICKEN HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CHIEFTAIN'S HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "CHITIN HELMET");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CIGAR");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CIRCUS DIRECTOR'S TOPHAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CLAP CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CLAY PIPE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CLOGS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CLOTHIER'S THIMBLE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CLOWNFISH HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CONQUISTADOR'S HELMET");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CORACLE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CROWN OF LOWER EGYPT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CROWN OF UPPER EGYPT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CUPCAKE HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "CUTTHROAT CUIRASS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'WEAPON'), "CUTTHROAT KNUCKLES");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "CYLINDER HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "DEV CAPE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "DEVIL'S DIADEM");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'TOOL'), "DOWSING ROD");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "DRAGON HELM");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "DRUID'S CLOAK");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "DRUID'S HELM");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "DRUID'S RING");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "DRUM & STICKS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "DUCK CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "DUSTMAN'S STOVEPIPE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "ELF'S CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "EXQUISITE BELT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FAMILY HEIRLOOM");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FEATHER BAND");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FEATHER SUNFEATHER");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FEATHERED TRICORNE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FELT HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FEZ");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FIDDLE & BOW");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FIREFIGHTER'S HELMET");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FISHERMAN'S HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FLOWER PLUSH");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FLUTE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FORGE RING");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FOUR WINDS HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FOX HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FREEBOOTER'S SLOUCH");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FROG PRINCE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'TOOL'), "FRYING PAN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FULL METAL HELMET");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FUR BOOTS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "FUR CLOAK");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "GAME TIME TOKEN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "GAUZE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "GILDABLE EQUIPMENT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "GOAT MASK");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "GOGGLED HELMET");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "GOVERNOR'S TRICORNE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "GRAND BELT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "GRAND TROLL HELM");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "HALLOWEEN DERBY");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "HARDHAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "HARMONICA");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "HEADSET");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "HEMP PANTS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "HEMP SHIRT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "HERALDIC CAPE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "HERMINE CAPE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "HERMINE CLOAK");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "HIDE CLOAK");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "HIGHLAND BONNET");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "HIRDSMAN'S CAPE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "HIRDSMAN'S HELMET");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "HOLY HALO");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "HORSE NOMAD'S HELM");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "HUNTER'S BELT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'WEAPON'), "HUNTER'S BOW");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "HUNTER'S QUIVER");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "HUNTER'S SHIRT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "HUSSAR'S WINGS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "INSTRUMENTS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "IRRLANTERN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "JACK-O'-MASK");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "KOZHUKH");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "LADDIE'S CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "LANTERN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "LAUNCH HELMET");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "LAWSPEAKER'S HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "LAWSPEAKER'S ROBES");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "LEATHER ARMOR");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "LEATHER BACKPACK");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "LEATHER BOOTS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "LEATHER COAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "LEATHER MERCHANT'S HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "LEATHER PANTS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "LEECH");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "LEEK");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "LEPRECHAUN'S HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "LINEN PANTS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "LINEN SHIRT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "LUTE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "LYNX CAPE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'WEAPON'), "LYNX CLAW GLOVES");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "MAGICIAN'S HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "MAMMOTH GUARD");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "MARTIAN'S HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "MASK OF THE GREEN MAN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "MELON HELM");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "MERCHANT'S PANTS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "MERCHANT'S RING");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "MERCHANT'S ROBE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARROW'), "METAL ARROW");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'TOOL'), "METAL AXE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'TOOL'), "METAL SAW");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'TOOL'), "METAL SHOVEL");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "MINER'S HELM");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "MOHAIR SHIRT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "MOLEHIDE PANTS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "MONOCLE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "MOOSE HIDE JACKET");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "MUSHROOM HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "NAVY BICORNE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "NETTLE PANTS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "NETTLE SHIRT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "NURSE HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "NUT BERET");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "OCCULT RING");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PACKAGE HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PARASOL MUSHROOM HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PEARL NECKLACE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PHARAO'S CROWN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PHRYGIAN CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'TOOL'), "PICKAXE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PICKELHAUBE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PILGRIM'S HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PIMP HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PINEAPPLE PANAMA");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PINK BOW");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PIRATE CAPTAIN'S HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PLAIN TABARD");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "PLATE ARMOR");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "PLATE BOOTS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "PLATE GAUNTLETS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "PLATE GREAVES");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "PLATE HELMET");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PLUMBER'S CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PLUMBER'S PLUNGER");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "POINTED CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "POOR MAN'S BELT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "POOR MAN'S GLOVES");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'FISHING'), "PRIMITIVE CASTING-ROD");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PRINCESS' CONE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PROPELLER CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "PROTEST SIGN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "QUEST GIVER'S STUMP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "RAIDER'S CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "RAINBOW SHELL AMULET");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "RANGER'S BOOTS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'WEAPON'), "RANGER'S BOW");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "RANGER'S CAPE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "RANGER'S PANTS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "RANGER'S SHIRT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "RASTA CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "RAT HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "RATTLESNAKE STETSON");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "RAVEN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "REINDEER CAPE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "REINDEER PARKA");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "RING OF BRODGAR (JEWELRY)");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "ROBIN HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "ROPE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "ROYAL CROWN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "RUDOLF KIT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "RUSALKA'S WATER MOCCASINS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SAFARI HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SAILOR'S CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SANTA'S CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SAUCEPAN HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SCARLET GOWN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SCYTHE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SEAL HIDE HOSES");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SEER'S HOOD");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SEER'S SHOES");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SEER'S TUNIC");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SHEARS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SHERLOCK HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SHERRIF'S HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SILK GLOVES");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SILVER FOR THE FERRYMAN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SLEDGEHAMMER");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'WEAPON'), "SLING");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SMITHY'S HAMMER");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SNAKESKIN BELT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SNAKESKIN BOOTS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SOMBRERO");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SPECTACLES");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SPRUCECAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SQUID HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "STAHLHELM");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "STONE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARROW'), "STONE ARROW");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'TOOL'), "STONE AXE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "STRAW CAPE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "STRAW HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "STRAWBERRY HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "STRAWBERRY SANTA");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SULTAN'S TURBAN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SUN VISOR");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "SWAN FEATHER CAPE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "THANE'S HELM");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "THANE'S RING");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "THE ABOMINABLE SNOWMAN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "THE GREEN HERO'S CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "THE PERFECT HOLE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "THINKING CAP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "TINFOIL HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "TOGA");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "TORCH");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "TRAFFIC CONE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "TRAVELLER'S SACK");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "TROLL BELT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "TSOKSHA");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "TURKEY TOPPER");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "UNIVERSITY MORTARBOARD");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "VALENTINE'S CYLINDER");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "VALENTINE'S TOP");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "VAPNTREYIU");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "VICTORIAN BOWLER");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "VIKING HELM");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "VÖLVA'S WAND");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "WALRUS BOOTS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "WALRUS CAPE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "WANDERER'S BINDLE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "WEAPONS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "WELDING MASK");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "WICKER PICKER");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "WINGED HELMET");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "WITCH HAT");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "WOLF CAPE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "WOLVERINE BOOTS");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'ARMOR'), "WOODEN ROUNDSHIELD");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'TOOL'), "WOODEN SHOVEL");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "WOODLAND CROWN");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'TOOL'), "WOODSMAN'S AXE");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "WOODSMAN'S TUNIC");
INSERT OR IGNORE INTO item (item_type_id, name_key) VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "WOODSMAN'S USHANKA");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "WOOL PANTS");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "WOOL SHIRT");
INSERT OR IGNORE INTO item (item_type_id, name_key)
VALUES ((SELECT item_type_id FROM item_type WHERE name_key = 'EQUIPMENT'), "YULE TOP");

INSERT OR IGNORE INTO item_contents
VALUES ((SELECT item_id FROM item WHERE name_key = 'WATERSKIN'), 3.0, 0.0, 0.0);
INSERT OR IGNORE INTO item_contents
VALUES ((SELECT item_id FROM item WHERE name_key = 'WATERFLASK'), 2.0, 0.0, 0.0);
INSERT OR IGNORE INTO item_contents
VALUES ((SELECT item_id FROM item WHERE name_key = 'BIRCHBARK KUKSA'), 0.4, 0.0, 0.0);
INSERT OR IGNORE INTO item_contents
VALUES ((SELECT item_id FROM item WHERE name_key = 'BUCKET'), 10.0, 10.0, 1000.0);

INSERT OR IGNORE INTO item_equipable
SELECT item_id
FROM item
WHERE item_type_id in (SELECT item_type_id FROM item_type WHERE name_key in ('EQUIPMENT', 'TOOL', 'ARMOR', 'WEAPON'));



CREATE TABLE IF NOT EXISTS forageable
(
    name     TEXT PRIMAY KEY,
    inv_res  TEXT    NOT NULL,
    game_res TEXT    NOT NULL,
    min_val  INTEGER NOT NULL,
    max_val  INTEGER NOT NULL,
    location TEXT    NOT NULL
)

INSERT OR IGNORE INTO forageable
VALUES ("Blueberries", "gfx/terobjs/items/blueberry", "gfx/terobjs/blueberry", 5, 20, "Forests, Grasslands"),
       ("Perfect Autumn Leaf", "gfx/invobjs/herbs/perfectautumnleaf", "", 10, 40, "Forests"),
       ("Spindly Taproot", "gfx/invobjs/herbs/spindlytaproot", "", 10, 40, "Forests"),
       ("Elven Lights", "gfx/invobjs/herbs/yulelights", "", 10, 40, "Forests (Winter)"),
       ("Dandelion", "res/gfx/terobjs/items/dandelion", "", 10, 40, "Grassland, Forests"),
       ("Mistletoe", "gfx/invobjs/herbs/mistletoe", "", 10, 40, "Forests"),
       ("Coltsfoot", "gfx/invobjs/herbs/coltsfoot", "", 10, 40, "Leaf Patch, Ox Pasture, Green Brake"),
       ("Stinging Nettle", "gfx/invobjs/herbs/stingingnettle", "", 25, 100, "Forests"),
       ("Chives", "gfx/invobjs/herbs/chives", "", 50, 200, "Smobre Bramble"),
       ("Chantrelles", "gfx/terobjs/items/chantrelle", "", 52, 208, "Forests"),
       ("Wild Windsown Weed", "gfx/invobjs/herbs/windweed", "", 80, 320, "Forests, Grassland, Swamp"),
       ("Dewy Lady's Mantle", "gfx/invobjs/herbs/ladysmantledew", "", 80, 320, "Swamp, Bog, Grassland"),
       ("Lady's Mantle", "gfx/invobjs/herbs/ladysmantle", "", 80, 320, "Bog, Swamp, Grassland, Fen"),
       ("Thorny Thistle", "gfx/invobjs/herbs/thornythistle", "", 90, 360, "Dirt"),
       ("Gooseneck Barnacle", "gfx/invobjs/herbs/goosebarnacle", "", 100, 400, "Skargard"),
       ("Heartsease", "gfx/invobjs/herbs/heartsease", "", 100, 400, "Grass"),
       ("Stalagoom", "gfx/invobjs/herbs/stalagoom", "", 100, 400, "Cave"),
       ("Lingonberries", "gfx/invobjs/herbs/lingon", "", 108, 432, "Forest"),
       ("Peculiar Flotsam", "gfx/invobjs/herbs/flotsam", "", 110, 440, "Water"),
       ("Bloated Bolete", "gfx/invobjs/herbs/bloatedbolete", "", 110, 440, "Forest, Grassland"),
       ("Four-Leaf Clover", "gfx/invobjs/herbs/fourleafclover", "", 130, 520,
        "Moor, Highground, Leaf Patch, Flower Meadow"),
       ("Clover", "gfx/invobjs/herbs/fourleafclover", "", 130, 520, "Moor, Highground, Leaf Patch, Flower Meadow"),
       ("Uncommon Snapdragon", "gfx/invobjs/herbs/snapdragon", "", 144, 576,
        "Moor, Lush Field, Red Plain, Greens Wald, Highground"),
       ("Parasol Mushroom", "gfx/invobjs/herbs/parasolshroom", "", 150, 600, "Lichen Wold, Cloud Range, Shady Corpse"),
       ("Dusk Fern", "gfx/invobjs/herbs/duskfern", "", 150, 600, "Cave"),
       ("Morels", "", "", 150, 600, "Forests"),
       ("Waybroad", "gfx/invobjs/herbs/waybroad", "", 150, 600,
        "Bog, Fen, Swamp, Grassland, Highground, Timber Land, Red Plain"),
       ("Candleberry", "gfx/invobjs/herbs/candleberry", "", 160, 640, "Swamp, Bog, Fen"),
       ("Yellowfeet", "gfx/invobjs/herbs/yellowfoot", "", 200, 800, "Forests"),
       ("Yarrow", "gfx/invobjs/herbs/yarrow", "", 200, 800, "Grassland"),
       ("Lamp Stalk", "gfx/invobjs/herbs/lampstalk", "", 200, 800, "Water"),
       ("Spirited Mandrake Root", "gfx/invobjs/mandrakespirited", "", 200, 800, "Moor"),
       ("Mandrake Root", "gfx/invobjs/herbs/mandrake", "", 200, 800, "Moor, Pine Barren"),
       ("Cattail", "gfx/invobjs/herbs/cattailfibre", "", 210, 840, "Shallow Water, Bog, Swamp, Fen"),
       ("Swamplily", "gfx/terobjs/items/leaf-swamplily", "", 250, 1000, "Shallow Water, Bog, Fen, Swamp"),
       ("Washed-up Bladderwrack", "gfx/invobjs/herbs/bladderwrack", "", 250, 1000, "Beach, Skargard"),
       ("Gray Clay", "gfx/terobjs/items/clay-gray", "", 270, 1080, "Shallow Water"),
       ("Tangled Bramble", "gfx/invobjs/herbs/tangledbramble", "", 280, 1120, "Dirt"),
       ("Royal Toadstool", "gfx/invobjs/herbs/royaltoadstool", "", 324, 1296, "Swamp"),
       ("Frog's Crown", "gfx/invobjs/herbs/frogscrown", "", 450, 1800, "Mountain, Swamp"),
       ("Giant Puffball", "gfx/invobjs/herbs/giantpuffball", "", 500, 2000,
        "Root Bosk, Greens Ward, Leaf Patch, Blue Sod"),
       ("Rustroot", "gfx/invobjs/herbs/rustroot", "", 500, 2000, "Forests"),
       ("Chiming Bluebell", "gfx/invobjs/herbs/chimingbluebell", "", 520, 2080, "Swamp, Grassland, Cloud Range"),
       ("Cavebulb", "gfx/invobjs/herbs/cavebulb", "", 525, 2100, "Cave"),
       ("Razor Clam", "gfx/invobjs/herbs/razorclams-b", "", 600, 2400, "Beach"),
       ("Common Starfish", "gfx/invobjs/herbs/commonstarfish", "", 700, 2800, "Beach, Skarguard"),
       ("River Pearl Mussel", "gfx/invobjs/herbs/mussels", "", 760, 3040, "Shallow Water"),
       ("Bay Bolete", "gfx/invobjs/herbs/baybolete", "", 960, 3840, "Sombre Bramble, Greens Ward, Oak Wilds, Dry Flat"),
       ("Rainbowpad", "gfx/invobjs/rainbowpad", "", 1000, 4000, "Swamp"),
       ("Frogspawn", "gfx/invobjs/herbs/frogspawn", "", 1000, 4000, "Shallow Water"),
       ("Blood Stern", "gfx/invobjs/herbs/bloodstern", "", 1250, 5000, "Forest"),
       ("Glimmermoss", "gfx/invobjs/herbs/glimmermoss", "", 1275, 5100, "Cave"),
       ("Edelweiss", "gfx/invobjs/herbs/edelweiss", "", 1600, 6400, "Mountain"),
       ("Strawberry", "gfx/invobjs/herbs/strawberry", "", 1850, 7400,
        "Flower Meadow, Sombre Bramble, Greens Ward, Deep Tangle, Cloud Range"),
       ("Rainbow Shell", "gfx/invobjs/herbs/seashell", "", 2500, 10000, "Beach, Shallow Water"),
       ("Kvann", "gfx/invobjs/herbs/kvann", "", 2500, 10000, "Root Bosk, Leaf Patch,  Oak Wilds"),
       ("Camomile", "gfx/invobjs/herbs/camomile", "", 3300, 13200, "Bog, Swamp, Fen, Flower Meadow"),
       ("Dill", "gfx/invobjs/herbs/dill", "", 3500, 14000, "Blue Sod, Lush Field"),
       ("Sage", "", "", 3500, 14000, "Greens Ward, Beech Grove"),
       ("Thyme", "gfx/invobjs/herbs/thyme", "", 3500, 14000, "Red Plain, Leaf Patch"),
       ("Black Trumpets", "gfx/invobjs/herbs/blacktrumpet", "", 3500, 14000, "Beech Grove, Pine Barren, Lichen  Wold"),
       ("Green Kelp", "gfx/invobjs/herbs/greenkelp", "", 3500, 14000, "Water, Beach"),
       ("Cave Coral", "gfx/invobjs/herbs/cavecoral", "", 3692, 14768, "Cave"),
       ("Oyster", "gfx/invobjs/herbs/oyster", "", 4000, 16000, "Shallow Water"),
       ("Cave Clay", "gfx/invobjs/clay-cave", "", 4600, 18400, "Cave"),
       ("Field Blewits", "gfx/invobjs/herbs/fieldblewit", "", 5000, 20000,
        "Beech Grove, Cloud Range, Root Bosk, Dry Flat"),
       ("Cave Lantern", "gfx/invobjs/herbs/cavelantern", "", 8000, 32000, "Cave"),
       ("Wintergreen", "gfx/invobjs/herbs/wintergreen", "", 12000, 48000, "Greens Ward, Moss Brush"),
       ("Marsh-Mallow", "gfx/invobjs/herbs/marshmallow", "", 12500, 50000,
        "Beech Grove, Fen, Lichen Wold, Sombre Bramble, Bog, Swamp, Shady Corpse"),
       ("Indigo Cap", "", "", 39000, 156000, "Cave");