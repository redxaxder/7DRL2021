Escape from Dump Moon Detritus-IV
 (EDMD4)

Notes!v2


==Theme==
science fiction
- goal: escape from garbage-planet

==UX==
mouse for attacking
click and drag for surgery!
keyboard or mouse for movement
images
  - terrain?
  - enemies?
  - items (weapons)?
    - there are 3 inventory slots for weapons
    - when walking over a weapon
      - if you have an empty slot, the weapon goes there
      - otherwise, it swaps with currently selected weapon
      - weapon slots are bound to 1/2/3 and have clickable buttons
  - "organs"
sound effects
  - what plays a sound?
  - weapon plays a sound
    - if the attack hits armor, play armor sound shortly after
  - surgery plays a sound?
animations
  - interpolate movement
  - flash decaying items
  - hitting health (superimpose partially transparent static image of sparks and smoke)
  - hitting armor (small sparkle)
targeting
  - mousing over an enemy puts their info on the CONTEXT AREA
  - clicking on an ememy marks them as selected
  - if there is a selected enemy (and no moused over enemy)
    the selected enemy is on the CONTEXT AREA
  - clicking on the target board attacks the selected (if in range)
  - indicate affected cells for AoE weapon


=== mockup ===

===========================================================================
|             |                                         |   CONTEXT AREA  |
| Player board|                                         |                 |
|             |                                         |                 |
|             |                                         |                 |
|             |                                         |                 |
|             |           r                             |                 |
|-------------|                                         |    ######       |
|             |           r                             |    ######       |
|             |                                         |    ######       |
|             |                                         |                 |
|             |                                         |                 |
|  Help text  |                                         |                 |
|             |                                         |                 |
|             |                                         |                 |
|             |              ###                        |                 |
|             |               x#                        |                 |
|             |              @ .                        |                 |
|             |                                         |                 |
|             |                                         |                 |
|             |                                         |                 |
|             |                                         |                 |
|             |                                         |                 |
|             |                                         |                 |
===========================================================================


player board
|------|
|4h 2a |
|------|
|######|
|####0#|
|######|
|##1###|
|####2#|
|######|
|------|
|WeapName
|------|
|W1W2W3| <- clickable tabs with icons of the weapons, selected one highlighted
|1 2 3 |  <- clickable number buttons ala minesweeper cells. maybe color shifted?
|------|


target board
|---------|
| NAME    |
|---------|
| 4h  2a  |
|=========|
| LEGEND  |
|=========|
|     1 1 |
|  |------|
|  |######|
| 0|####0#|
|  |######|
| 0|##1###|
| 1|####2#|
|  |######|
|--|------|
||      |||
||      |||  << attack shape
||XXXXXX|||
||      |||
||      |||
||      |||
|---------|



== Arc of the game==
~20 minute session length
surgery room between levels
  - it contains a few organs to swap out
find new weapons in levels


== Moment to moment gameplay ==
minesweeper combat on enemies
  - wounded cells provide clues
  - attacking a cell wounds it
  - wounding all health-cells kills the enemy
  - clues:
    - basic clue (red) is a number in a cell counting the number of adj health
    - armor clue (blue) counts the number of adj. armor cells
    - mixed clue (purple) counts both
    - if there is a clue in a cell for which you do not have the required sensory
      equipment, a ? of that color is displayed instead.

    - a (red) row (resp. column) clue counts the number of (health) in the row
       - resp (blue/armor) and (purple/mixed)


    - region clues:
      - if these are enabled, some of the cells are somehow given colors
      - for a given color, a (red/blue/purple) region clue
        gives the number of (health/armor/mixed) cells
      - the region clues are in the LEGEND


enemies start out damaged: there are already some clues visible



== Items ==
item groups:
  weapons:
    give attack patterns
  sensory organs:
    give clues
  armor plating:
    give defense
  life support:
    give hp


==== Sensory ====
all sensory organs are 1x1

eyes: enable red cell clues
ultraviolet eye: enable blue cell clues
______         : enable purple cell clues
millimeter wave scanner:
  enable row clues on rows with damaged cells
backscatter x-ray scanner:
  enable column clues on columns with damaged cells

==== Weapon ====

AOE weapons:
  copy clicked region attack onto the other targets

images:
  single "weapon" image recycled accross all weapons


NAME            | RANGE      | TARGETS    | SHAPE
pistol          | 3          | 1 cell     | 1x1
hollow point    | 3          | 1 cell     | X
rifle           | 6          | 1 cell     | 1x1
laser drill     | 1          | 1 cell     | 3x3 AP
cluster grenade | 3          | 5x5        | 1x1
frag grenade    | 3          | 3x3        | 3x3
shotgun         | 3          | clever AoE | 1x1
plasma cutter   | 1          | 1 cell     | 6x1
laser sword     | 1          | 1 cell     | 1x6
chainsaw        | 1          | 1 cell     | 6x6
machine gun     | 3          | 1 cell     | 1x1 (cascades to adjacent when revealing 0)

x x
 x
x x

AP: armor piercing. treats armor cells like ordinary cells

==== Enemies ====
An enemy attacks you. What square do they hit?
  - enemies have attack shapes. they randomly select a possible placement
    of that shape and attack there.
  - enemy always tries to include a square that is undamaged


differentiating enemies
  - different names
  - different attack shapes
  - different hp amounts
  - different armor amounts
  - different starting damage


murderous vacuum robot
  - hp: 2
  - armor: 0
  - damage: 10
  - attack: 1 cell

Name                    | HP   | Armor | Attack | Damage | Special
murderous vacuum robot  | 2    | 0     | 1x1    | 10     |
retired battlebot       | 5    | 3     | 1x3    | 5      |
kamikaze drone          | 1    | 0     | 6x6    | 8      | self destructs
mobile refrigerator     | 2    | 8     | 2x2    | 10     |



==== Armor ====
plating:
  1x1
  2x2
  2x3
  etc...

==== Life support ====
life support items are disabled if any of their cells are injured

images:
  - working organ
  - nonworking organ (same image but color shifted)
heart: 2x2
kidney: 2x2
series 7 sports heart: 2x1
4-cylinder turbo heart: 1x1
globular nutrient cluster: 1x1



== Things we would like to know how to do before go time ==
- drawing stuff
  - rendering tiles (done in chicken scratch)
  - rendering text (not done in chicken scratch)
  - animating (partly in place)
- playing sounds
- clicking and dragging
- main loop. two loops. (done in chicken)
- random numbers (done in chicken)
- gotta find them pictures














Milestone 0

UX:
enemy targetting
 - select
 - display
 - attack

minesweeper clues

weapons:
  - no weapon indication. only one weapon

Enemies:
 - basic enemies without special abilities
 - murderous vacuum robot

Map:
  - manual default map

vision:
  - full vision of everything

images:
  - roomba
  - player
  - hp <-
  - floor (1 image)
  - wall (1 image)
  - player organ
  - minesweeper square (unopened)
    - (opened)

sounds:
  - attack sound

animations:
  - interpolated movement

Milestone 1

surgery
  - surgery room
  - click and drag
  - basic organs
    - organs that give hp
  - surgery sound
  - clues in context area

item pickups
  - health
    - use hp icon?
  - item decay

enemies:
  - kamikaze drone (+image)

ux:
  - color shifting
  - distinguish between
    - never seen / currently seen / previously seen terrain
  - image for exit

vision:
  - full view of room you're in
  - (both rooms if you're in a doorway)

basic map gen
  - rectangle packing
  - piles of crap added on top
  - punch holes between rectangles randomly

animations:
  - item decay warning

winning

Milestone 2

weapons
  - item pickups
  - weapon selection
  - weapon swapping
  - implementations of individual weaons
    - pistol (starting weapon
    - plasma cutter
    - chainsaw
    - laser sword
    - rifle

ux
  - weapon targetting highlights

armor
  - enemies with armor
  - armor sound

enemies
  - fridge (+image)
  - battlebot (+image)

images
  - purple clues
  - blues clues
  - concealed clues
  - webcam/infracam/scanners/armor plates

sensory organs
  - start with basic eyes (can see red clues. no purple/blue clues)
  - webcam (can see red clues)
  - infrared camera (can see purple/blue clues)
  - millimeter wave scanner
    - row clues (all types that you can see)
  - backscatter x-ray scanner
    - column clues (all types that you can see)

Milestone 3
  - fancy weapons
    - cluster grenade
    - frag grenade
    - shotgun
    - laser drill
    - hollow point

  - more enemies!

Milestone 4
  - infinite sounds!
  - fancy animations!
    - flying sparks and stuff
  - region clues?
  - cosmetic trash
  - fun messages
    - oh that cant be sanitary

game log



























