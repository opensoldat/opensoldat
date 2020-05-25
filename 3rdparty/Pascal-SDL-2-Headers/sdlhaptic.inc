//from "sdl_haptic.h"

{**
 *
 *   The SDL Haptic subsystem allows you to control haptic (force feedback)
 *   devices.
 *
 *  The basic usage is as follows:
 *   - Initialize the Subsystem (::SDL_INIT_HAPTIC).
 *   - Open a Haptic Device.
 *    - SDL_HapticOpen() to open from index.
 *    - SDL_HapticOpenFromJoystick() to open from an existing joystick.
 *   - Create an effect (::SDL_HapticEffect).
 *   - Upload the effect with SDL_HapticNewEffect().
 *   - Run the effect with SDL_HapticRunEffect().
 *   - (optional) Free the effect with SDL_HapticDestroyEffect().
 *   - Close the haptic device with SDL_HapticClose().
 *
 *  Simple rumble example:
 *
 *    SDL_Haptic *haptic;
 *
 *    // Open the device
 *    haptic = SDL_HapticOpen( 0 );
 *    if (haptic == NULL)
 *       return -1;
 *
 *    // Initialize simple rumble
 *    if (SDL_HapticRumbleInit( haptic ) != 0)
 *       return -1;
 *
 *    // Play effect at 50% strength for 2 seconds
 *    if (SDL_HapticRumblePlay( haptic, 0.5, 2000 ) != 0)
 *       return -1;
 *    SDL_Delay( 2000 );
 *
 *    // Clean up
 *    SDL_HapticClose( haptic );
 *
 *
 *  Complete example:
 *
 * int test_haptic( SDL_Joystick * joystick )
 *    SDL_Haptic *haptic;
 *    SDL_HapticEffect effect;
 *    int effect_id;
 *
 *    // Open the device
 *    haptic = SDL_HapticOpenFromJoystick( joystick );
 *    if (haptic == NULL) return -1; // Most likely joystick isn't haptic
 *
 *    // See if it can do sine waves
 *    if ((SDL_HapticQuery(haptic) & SDL_HAPTIC_SINE)==0)
 *       SDL_HapticClose(haptic); // No sine effect
 *       return -1;
 *
 *
 *    // Create the effect
 *    memset( &effect, 0, sizeof(SDL_HapticEffect) ); // 0 is safe default
 *    effect.type = SDL_HAPTIC_SINE;
 *    effect.periodic.direction.type = SDL_HAPTIC_POLAR; // Polar coordinates
 *    effect.periodic.direction.dir[0] = 18000; // Force comes from south
 *    effect.periodic.period = 1000; // 1000 ms
 *    effect.periodic.magnitude = 20000; // 20000/32767 strength
 *    effect.periodic.length = 5000; // 5 seconds long
 *    effect.periodic.attack_length = 1000; // Takes 1 second to get max strength
 *    effect.periodic.fade_length = 1000; // Takes 1 second to fade away
 *
 *    // Upload the effect
 *    effect_id = SDL_HapticNewEffect( haptic, &effect );
 *
 *    // Test the effect
 *    SDL_HapticRunEffect( haptic, effect_id, 1 );
 *    SDL_Delay( 5000); // Wait for the effect to finish
 *
 *    // We destroy the effect, although closing the device also does this
 *    SDL_HapticDestroyEffect( haptic, effect_id );
 *
 *    // Close the device
 *    SDL_HapticClose(haptic);
 *
 *    return 0; // Success
 *
 *
 *
 * You can also find out more information on my blog:
 * http://bobbens.dyndns.org/journal/2010/sdl_haptic/
 *
 *  Edgar Simo Serra
 *}

  {**
   *   SDL_Haptic
   *
   *  The haptic structure used to identify an SDL haptic.
   *
   *   SDL_HapticOpen
   *   SDL_HapticOpenFromJoystick
   *   SDL_HapticClose
   *}
type
  PSDL_Haptic = ^TSDL_Haptic;
  TSDL_Haptic = record end;

  {**
   *   Haptic features
   *
   *  Different haptic features a device can have.
   *}

  {**
   *   Haptic effects
   *}

  {**
   *   Constant effect supported.
   *
   *  Constant haptic effect.
   *
   *   SDL_HapticCondition
   *}
const
  SDL_HAPTIC_CONSTANT = (1 shl 0);

  {**
   *   Sine wave effect supported.
   *
   *  Periodic haptic effect that simulates sine waves.
   *
   *   SDL_HapticPeriodic
   *}
const
  SDL_HAPTIC_SINE     = (1 shl 1);

  {**
   *   Square wave effect supported.
   *
   *  Periodic haptic effect that simulates square waves.
   *
   *   SDL_HapticPeriodic
   *}
const
  SDL_HAPTIC_SQUARE   = (1 shl 2);

  {**
   *   Triangle wave effect supported.
   *
   *  Periodic haptic effect that simulates triangular waves.
   *
   *   SDL_HapticPeriodic
   *}
const
  SDL_HAPTIC_TRIANGLE = (1 shl 3);

  {**
   *   Sawtoothup wave effect supported.
   *
   *  Periodic haptic effect that simulates saw tooth up waves.
   *
   *   SDL_HapticPeriodic
   *}
const
  SDL_HAPTIC_SAWTOOTHUP = (1 shl 4);

  {**
   *   Sawtoothdown wave effect supported.
   *
   *  Periodic haptic effect that simulates saw tooth down waves.
   *
   *   SDL_HapticPeriodic
   *}
const
  SDL_HAPTIC_SAWTOOTHDOWN = (1 shl 5);

  {**
   *   Ramp effect supported.
   *
   *  Ramp haptic effect.
   *
   *   SDL_HapticRamp
   *}
const
  SDL_HAPTIC_RAMP = (1 shl 6);

  {**
   *   Spring effect supported - uses axes position.
   *
   *  Condition haptic effect that simulates a spring.  Effect is based on the
   *  axes position.
   *
   *   SDL_HapticCondition
   *}
const
  SDL_HAPTIC_SPRING = (1 shl 7);

  {**
   *   Damper effect supported - uses axes velocity.
   *
   *  Condition haptic effect that simulates dampening.  Effect is based on the
   *  axes velocity.
   *
   *   SDL_HapticCondition
   *}
const
  SDL_HAPTIC_DAMPER = (1 shl 8);

  {**
   *   Inertia effect supported - uses axes acceleration.
   *
   *  Condition haptic effect that simulates inertia.  Effect is based on the axes
   *  acceleration.
   *
   *   SDL_HapticCondition
   *}
const
  SDL_HAPTIC_INERTIA = (1 shl 9);

  {**
   *   Friction effect supported - uses axes movement.
   *
   *  Condition haptic effect that simulates friction.  Effect is based on the
   *  axes movement.
   *
   *   SDL_HapticCondition
   *}
const
  SDL_HAPTIC_FRICTION = (1 shl 10);

  {**
   *   Custom effect is supported.
   *
   *  User defined custom haptic effect.
   *}
const
  SDL_HAPTIC_CUSTOM = (1 shl 11);

  {*Haptic effects*}

  {* These last few are features the device has, not effects *}

  {**
   *   Device can set global gain.
   *
   *  Device supports setting the global gain.
   *
   *   SDL_HapticSetGain
   *}
const
  SDL_HAPTIC_GAIN = (1 shl 12);

  {**
   *   Device can set autocenter.
   *
   *  Device supports setting autocenter.
   *
   *   SDL_HapticSetAutocenter
   *}
const
  SDL_HAPTIC_AUTOCENTER = (1 shl 13);

  {**
   *   Device can be queried for effect status.
   *
   *  Device can be queried for effect status.
   *
   *   SDL_HapticGetEffectStatus
   *}
const
  SDL_HAPTIC_STATUS = (1 shl 14);

  {**
   *   Device can be paused.
   *
   *   SDL_HapticPause
   *   SDL_HapticUnpause
   *}
const
  SDL_HAPTIC_PAUSE = (1 shl 15);

  {**
   *  Direction encodings
   *}

  {**
   *   Uses polar coordinates for the direction.
   *
   *   SDL_HapticDirection
   *}
const
  SDL_HAPTIC_POLAR = 0;

  {**
   *   Uses cartesian coordinates for the direction.
   *
   *   SDL_HapticDirection
   *}
const
  SDL_HAPTIC_CARTESIAN = 1;

  {**
   *   Uses spherical coordinates for the direction.
   *
   *   SDL_HapticDirection
   *}
const
  SDL_HAPTIC_SPHERICAL = 2;

  {*Direction encodings*}

  {*Haptic features*}

  {*
   * Misc defines.
   *}

  {**
   *  Used to play a device an infinite number of times.
   *
   *  SDL_HapticRunEffect
   *}
const
  //SDL_HAPTIC_INFINITY = 4294967295U;
  SDL_HAPTIC_INFINITY = 4294967295; //right?!

  {**
   *   Structure that represents a haptic direction.
   *
   *  Directions can be specified by:
   *   - SDL_HAPTIC_POLAR : Specified by polar coordinates.
   *   - SDL_HAPTIC_CARTESIAN : Specified by cartesian coordinates.
   *   - SDL_HAPTIC_SPHERICAL : Specified by spherical coordinates.
   *
   *  Cardinal directions of the haptic device are relative to the positioning
   *  of the device.  North is considered to be away from the user.
   *
   *  The following diagram represents the cardinal directions:
   *
                   .--.
                   |__| .-------.
                   |=.| |.-----.|
                   |--| ||     ||
                   |  | |'-----'|
                   |__|~')_____('
                     [ COMPUTER ]


                       North (0,-1)
                           ^
                           |
                           |
      (1,0)  West <----[ HAPTIC ]----> East (-1,0)
                           |
                           |
                           v
                        South (0,1)


                        [ USER ]
                          \|||/
                          (o o)
                    ---ooO-(_)-Ooo---

   *
   *  If type is SDL_HAPTIC_POLAR, direction is encoded by hundredths of a
   *  degree starting north and turning clockwise.  ::SDL_HAPTIC_POLAR only uses
   *  the first dir parameter.  The cardinal directions would be:
   *   - North: 0 (0 degrees)
   *   - East: 9000 (90 degrees)
   *   - South: 18000 (180 degrees)
   *   - West: 27000 (270 degrees)
   *
   *  If type is SDL_HAPTIC_CARTESIAN, direction is encoded by three positions
   *  (X axis, Y axis and Z axis (with 3 axes)).  ::SDL_HAPTIC_CARTESIAN uses
   *  the first three dir parameters.  The cardinal directions would be:
   *   - North:  0,-1, 0
   *   - East:  -1, 0, 0
   *   - South:  0, 1, 0
   *   - West:   1, 0, 0
   *
   *  The Z axis represents the height of the effect if supported, otherwise
   *  it's unused.  In cartesian encoding (1, 2) would be the same as (2, 4), you
   *  can use any multiple you want, only the direction matters.
   *
   *  If type is SDL_HAPTIC_SPHERICAL, direction is encoded by two rotations.
   *  The first two dir parameters are used.  The dir parameters are as
   *  follows (all values are in hundredths of degrees):
   *   - Degrees from (1, 0) rotated towards (0, 1).
   *   - Degrees towards (0, 0, 1) (device needs at least 3 axes).
   *
   *
   *  Example of force coming from the south with all encodings (force coming
   *  from the south means the user will have to pull the stick to counteract):
   *
   *  SDL_HapticDirection direction;
   *
   *  // Cartesian directions
   *  direction.type = SDL_HAPTIC_CARTESIAN; // Using cartesian direction encoding.
   *  direction.dir[0] = 0; // X position
   *  direction.dir[1] = 1; // Y position
   *  // Assuming the device has 2 axes, we don't need to specify third parameter.
   *
   *  // Polar directions
   *  direction.type = SDL_HAPTIC_POLAR; // We'll be using polar direction encoding.
   *  direction.dir[0] = 18000; // Polar only uses first parameter
   *
   *  // Spherical coordinates
   *  direction.type = SDL_HAPTIC_SPHERICAL; // Spherical encoding
   *  direction.dir[0] = 9000; // Since we only have two axes we don't need more parameters.
   *
   *
   *   SDL_HAPTIC_POLAR
   *   SDL_HAPTIC_CARTESIAN
   *   SDL_HAPTIC_SPHERICAL
   *   SDL_HapticEffect
   *   SDL_HapticNumAxes
   *}
type
  TSDL_HapticDirection = record
    _type: UInt8;               {**< The type of encoding. *}
    dir: array[0..2] of SInt32; {**< The encoded direction. *}
  end;

  {**
   *   A structure containing a template for a Constant effect.
   *
   *  The struct is exclusive to the ::SDL_HAPTIC_CONSTANT effect.
   *
   *  A constant effect applies a constant force in the specified direction
   *  to the joystick.
   *
   *   SDL_HAPTIC_CONSTANT
   *   SDL_HapticEffect
   *}
type
  TSDL_HapticConstant = record
    {* Header *}
    _type: UInt16;                   {**< SDL_HAPTIC_CONSTANT *}
    direction: TSDL_HapticDirection; {**< Direction of the effect. *}

    {* Replay *}
    length: UInt32;          {**< Duration of the effect. *}
    delay: UInt16;           {**< Delay before starting the effect. *}

    {* Trigger *}
    button: UInt16;          {**< Button that triggers the effect. *}
    interval: UInt16;        {**< How soon it can be triggered again after button. *}

    {* Constant *}
    level: SInt16;           {**< Strength of the constant effect. *}

    {* Envelope *}
    attack_length: UInt16;   {**< Duration of the attack. *}
    attack_level: UInt16;    {**< Level at the start of the attack. *}
    fade_length: UInt16;     {**< Duration of the fade. *}
    fade_level: UInt16;      {**< Level at the end of the fade. *}
  end;

  {**
   *   A structure containing a template for a Periodic effect.
   *
   *  The struct handles the following effects:
   *   - SDL_HAPTIC_SINE
   *   - SDL_HAPTIC_SQUARE
   *   - SDL_HAPTIC_TRIANGLE
   *   - SDL_HAPTIC_SAWTOOTHUP
   *   - SDL_HAPTIC_SAWTOOTHDOWN
   *
   *  A periodic effect consists in a wave-shaped effect that repeats itself
   *  over time.  The type determines the shape of the wave and the parameters
   *  determine the dimensions of the wave.
   *
   *  Phase is given by hundredth of a cycle meaning that giving the phase a value
   *  of 9000 will displace it 25% of its period.  Here are sample values:
   *   -     0: No phase displacement.
   *   -  9000: Displaced 25% of its period.
   *   - 18000: Displaced 50% of its period.
   *   - 27000: Displaced 75% of its period.
   *   - 36000: Displaced 100% of its period, same as 0, but 0 is preferred.
   *
   *  Examples:
   *
      SDL_HAPTIC_SINE
        __      __      __      __
       /  \    /  \    /  \    /
      /    \__/    \__/    \__/

      SDL_HAPTIC_SQUARE
       __    __    __    __    __
      |  |  |  |  |  |  |  |  |  |
      |  |__|  |__|  |__|  |__|  |

      SDL_HAPTIC_TRIANGLE
        /\    /\    /\    /\    /\
       /  \  /  \  /  \  /  \  /
      /    \/    \/    \/    \/

      SDL_HAPTIC_SAWTOOTHUP
        /|  /|  /|  /|  /|  /|  /|
       / | / | / | / | / | / | / |
      /  |/  |/  |/  |/  |/  |/  |

      SDL_HAPTIC_SAWTOOTHDOWN
      \  |\  |\  |\  |\  |\  |\  |
       \ | \ | \ | \ | \ | \ | \ |
        \|  \|  \|  \|  \|  \|  \|

   *
   *   SDL_HAPTIC_SINE
   *   SDL_HAPTIC_SQUARE
   *   SDL_HAPTIC_TRIANGLE
   *   SDL_HAPTIC_SAWTOOTHUP
   *   SDL_HAPTIC_SAWTOOTHDOWN
   *   SDL_HapticEffect
   *}
type
  TSDL_HapticPeriodic = record
    { Header *}
    _type: UInt16;        {**< SDL_HAPTIC_SINE, SDL_HAPTIC_SQUARE,
                               SDL_HAPTIC_TRIANGLE, SDL_HAPTIC_SAWTOOTHUP or
                               SDL_HAPTIC_SAWTOOTHDOWN *}
    direction: TSDL_HapticDirection;  {**< Direction of the effect. *}

    {* Replay *}
    length: UInt32;          {**< Duration of the effect. *}
    delay: UInt16;           {**< Delay before starting the effect. *}

    {* Trigger *}
    button: UInt16;          {**< Button that triggers the effect. *}
    interval: UInt16;        {**< How soon it can be triggered again after button. *}

    {* Periodic *}
    period: UInt16;          {**< Period of the wave. *}
    magnitude: SInt16;       {**< Peak value. *}
    offset: SInt16;          {**< Mean value of the wave. *}
    phase: UInt16;           {**< Horizontal shift given by hundredth of a cycle. *}

    {* Envelope *}
    attack_length: UInt16;   {**< Duration of the attack. *}
    attack_level: UInt16;    {**< Level at the start of the attack. *}
    fade_length: UInt16;     {**< Duration of the fade. *}
    fade_level: UInt16;      {**< Level at the end of the fade. *}
  end;

  {**
   *   A structure containing a template for a Condition effect.
   *
   *  The struct handles the following effects:
   *   - SDL_HAPTIC_SPRING: Effect based on axes position.
   *   - SDL_HAPTIC_DAMPER: Effect based on axes velocity.
   *   - SDL_HAPTIC_INERTIA: Effect based on axes acceleration.
   *   - SDL_HAPTIC_FRICTION: Effect based on axes movement.
   *
   *  Direction is handled by condition internals instead of a direction member.
   *  The condition effect specific members have three parameters.  The first
   *  refers to the X axis, the second refers to the Y axis and the third
   *  refers to the Z axis.  The right terms refer to the positive side of the
   *  axis and the left terms refer to the negative side of the axis.  Please
   *  refer to the ::SDL_HapticDirection diagram for which side is positive and
   *  which is negative.
   *
   *   SDL_HapticDirection
   *   SDL_HAPTIC_SPRING
   *   SDL_HAPTIC_DAMPER
   *   SDL_HAPTIC_INERTIA
   *   SDL_HAPTIC_FRICTION
   *   SDL_HapticEffect
   *}
type
  TSDL_HapticCondition = record
    {* Header *}
    _type: UInt16;                    {**< SDL_HAPTIC_SPRING, SDL_HAPTIC_DAMPER,
                                           SDL_HAPTIC_INERTIA or SDL_HAPTIC_FRICTION *}
    direction: TSDL_HapticDirection;  {**< Direction of the effect - Not used ATM. *}

    {* Replay *}
    length: UInt32;                   {**< Duration of the effect. *}
    delay: UInt16;                    {**< Delay before starting the effect. *}

    {* Trigger *}
    button: UInt16;                   {**< Button that triggers the effect. *}
    interval: UInt16;                 {**< How soon it can be triggered again after button. *}

    {* Condition *}
    right_sat: array[0..2] of UInt16; {**< Level when joystick is to the positive side. *}
    left_sat: array[0..2] of UInt16;  {**< Level when joystick is to the negative side. *}
    right_coeff: array[0..2] of SInt16;  {**< How fast to increase the force towards the positive side. *}
    left_coeff: array[0..2] of SInt16;   {**< How fast to increase the force towards the negative side. *}
    deadband: array[0..2] of UInt16;     {**< Size of the dead zone. *}
    center: array[0..2] of SInt16;       {**< Position of the dead zone. *}
  end;

  {**
   *   A structure containing a template for a Ramp effect.
   *
   *  This struct is exclusively for the ::SDL_HAPTIC_RAMP effect.
   *
   *  The ramp effect starts at start strength and ends at end strength.
   *  It augments in linear fashion.  If you use attack and fade with a ramp
   *  the effects get added to the ramp effect making the effect become
   *  quadratic instead of linear.
   *
   *   SDL_HAPTIC_RAMP
   *   SDL_HapticEffect
   *}
type
  TSDL_HapticRamp = record
    {* Header *}
    _type: UInt16;                    {**< SDL_HAPTIC_RAMP *}
    direction: TSDL_HapticDirection;  {**< Direction of the effect. *}

    {* Replay *}
    length: UInt32;                   {**< Duration of the effect. *}
    delay: UInt16;                    {**< Delay before starting the effect. *}

    {* Trigger *}
    button: UInt16;                   {**< Button that triggers the effect. *}
    interval: UInt16;                 {**< How soon it can be triggered again after button. *}

    {* Ramp *}
    start: SInt16;                    {**< Beginning strength level. *}
    _end: SInt16;                     {**< Ending strength level. *}

    {* Envelope *}
    attack_length: UInt16;            {**< Duration of the attack. *}
    attack_level: UInt16;             {**< Level at the start of the attack. *}
    fade_length: UInt16;              {**< Duration of the fade. *}
    fade_level: UInt16;               {**< Level at the end of the fade. *}
  end;

  {**
   *   A structure containing a template for the ::SDL_HAPTIC_CUSTOM effect.
   *
   *  A custom force feedback effect is much like a periodic effect, where the
   *  application can define its exact shape.  You will have to allocate the
   *  data yourself.  Data should consist of channels * samples Uint16 samples.
   *
   *  If channels is one, the effect is rotated using the defined direction.
   *  Otherwise it uses the samples in data for the different axes.
   *
   *   SDL_HAPTIC_CUSTOM
   *   SDL_HapticEffect
   *}
type
  TSDL_HapticCustom = record
    {* Header *}
    _type: UInt16;                    {**< SDL_HAPTIC_CUSTOM *}
    direction: TSDL_HapticDirection;  {**< Direction of the effect. *}

    {* Replay *}
    length: UInt32;                   {**< Duration of the effect. *}
    delay: UInt16;                    {**< Delay before starting the effect. *}

    {* Trigger *}
    button: UInt16;                   {**< Button that triggers the effect. *}
    interval: UInt16;                 {**< How soon it can be triggered again after button. *}

    {* Custom *}
    channels: UInt8;                  {**< Axes to use, minimum of one. *}
    period: UInt16;                   {**< Sample periods. *}
    samples: UInt16;                  {**< Amount of samples. *}
    data: PUInt16;                    {**< Should contain channels*samples items. *}

    {* Envelope *}
    attack_length: UInt16;            {**< Duration of the attack. *}
    attack_level: UInt16;             {**< Level at the start of the attack. *}
    fade_length: UInt16;              {**< Duration of the fade. *}
    fade_level: UInt16;               {**< Level at the end of the fade. *}
  end;

  {**
   *   The generic template for any haptic effect.
   *
   *  All values max at 32767 (0x7FFF).  Signed values also can be negative.
   *  Time values unless specified otherwise are in milliseconds.
   *
   *  You can also pass SDL_HAPTIC_INFINITY to length instead of a 0-32767
   *  value.  Neither delay, interval, attack_length nor fade_length support
   *  SDL_HAPTIC_INFINITY.  Fade will also not be used since effect never ends.
   *
   *  Additionally, the SDL_HAPTIC_RAMP effect does not support a duration of
   *  SDL_HAPTIC_INFINITY.
   *
   *  Button triggers may not be supported on all devices, it is advised to not
   *  use them if possible.  Buttons start at index 1 instead of index 0 like
   *  the joystick.
   *
   *  If both attack_length and fade_level are 0, the envelope is not used,
   *  otherwise both values are used.
   *
   *  Common parts:
   *
   *  // Replay - All effects have this
   *  Uint32 length;        // Duration of effect (ms).
   *  Uint16 delay;         // Delay before starting effect.
   *
   *  // Trigger - All effects have this
   *  Uint16 button;        // Button that triggers effect.
   *  Uint16 interval;      // How soon before effect can be triggered again.
   *
   *  // Envelope - All effects except condition effects have this
   *  Uint16 attack_length; // Duration of the attack (ms).
   *  Uint16 attack_level;  // Level at the start of the attack.
   *  Uint16 fade_length;   // Duration of the fade out (ms).
   *  Uint16 fade_level;    // Level at the end of the fade.
   *
   *
   *
   *  Here we have an example of a constant effect evolution in time:
   *
      Strength
      ^
      |
      |    effect level -->  _________________
      |                     /                 \
      |                    /                   \
      |                   /                     \
      |                  /                       \
      | attack_level --> |                        \
      |                  |                        |  <---  fade_level
      |
      +--------------------------------------------------> Time
                         [--]                 [---]
                         attack_length        fade_length

      [------------------][-----------------------]
      delay               length

   *
   *  Note either the attack_level or the fade_level may be above the actual
   *  effect level.
   *
   *   SDL_HapticConstant
   *   SDL_HapticPeriodic
   *   SDL_HapticCondition
   *   SDL_HapticRamp
   *   SDL_HapticCustom
   *}
type
  PSDL_HapticEffect = ^TSDL_HapticEffect;
  TSDL_HapticEffect = record
    {* Common for all force feedback effects *}
    _type: UInt16;                  {**< Effect type. *}
    case UInt16 of
      0: (constant: TSDL_HapticConstant;);    {**< Constant effect. *}
      1: (periodic: TSDL_HapticPeriodic;);    {**< Periodic effect. *}
      2: (condition: TSDL_HapticCondition;);  {**< Condition effect. *}
      3: (ramp: TSDL_HapticRamp;);            {**< Ramp effect. *}
      4: (custom: TSDL_HapticCustom;);        {**< Custom effect. *}
  end;

  {* Function prototypes *}

  {**
   *   Count the number of haptic devices attached to the system.
   *
   *   Number of haptic devices detected on the system.
   *}
function SDL_NumHaptics: Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_NumHaptics' {$ENDIF} {$ENDIF};

  {**
   *   Get the implementation dependent name of a Haptic device.
   *
   *  This can be called before any joysticks are opened.
   *  If no name can be found, this function returns NULL.
   *
   *   device_index Index of the device to get its name.
   *   Name of the device or NULL on error.
   *
   *   SDL_NumHaptics
   *}
function SDL_HapticName(device_index: Integer): PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticName' {$ENDIF} {$ENDIF};

  {**
   *   Opens a Haptic device for usage.
   *
   *  The index passed as an argument refers to the N'th Haptic device on this
   *  system.
   *
   *  When opening a haptic device, its gain will be set to maximum and
   *  autocenter will be disabled.  To modify these values use
   *  SDL_HapticSetGain() and SDL_HapticSetAutocenter().
   *
   *   device_index Index of the device to open.
   *   Device identifier or NULL on error.
   *
   *   SDL_HapticIndex
   *   SDL_HapticOpenFromMouse
   *   SDL_HapticOpenFromJoystick
   *   SDL_HapticClose
   *   SDL_HapticSetGain
   *   SDL_HapticSetAutocenter
   *   SDL_HapticPause
   *   SDL_HapticStopAll
   *}
function SDL_HapticOpen(device_index: Integer): PSDL_Haptic cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticOpen' {$ENDIF} {$ENDIF};

  {**
   *   Checks if the haptic device at index has been opened.
   *
   *   device_index Index to check to see if it has been opened.
   *   1 if it has been opened or 0 if it hasn't.
   *
   *   SDL_HapticOpen
   *   SDL_HapticIndex
   *}
function SDL_HapticOpened(device_index: Integer): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticOpened' {$ENDIF} {$ENDIF};

  {**
   *   Gets the index of a haptic device.
   *
   *   haptic Haptic device to get the index of.
   *   The index of the haptic device or -1 on error.
   *
   *   SDL_HapticOpen
   *   SDL_HapticOpened
   *}
function SDL_HapticIndex(haptic: PSDL_Haptic): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticIndex' {$ENDIF} {$ENDIF};

  {**
   *   Gets whether or not the current mouse has haptic capabilities.
   *
   *   SDL_TRUE if the mouse is haptic, SDL_FALSE if it isn't.
   *
   *   SDL_HapticOpenFromMouse
   *}
function SDL_MouseIsHaptic: Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_MouseInHaptic' {$ENDIF} {$ENDIF};

  {**
   *   Tries to open a haptic device from the current mouse.
   *
   *   The haptic device identifier or NULL on error.
   *
   *   SDL_MouseIsHaptic
   *   SDL_HapticOpen
   *}
function SDL_HapticOpenFromMouse: PSDL_Haptic cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticOpenFromMouse' {$ENDIF} {$ENDIF};

  {**
   *   Checks to see if a joystick has haptic features.
   *
   *   joystick Joystick to test for haptic capabilities.
   *   1 if the joystick is haptic, 0 if it isn't
   *   or -1 if an error ocurred.
   *
   *   SDL_HapticOpenFromJoystick
   *}
function SDL_JoystickIsHaptic(joystick: PSDL_Joystick): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_JoystickIsHaptic' {$ENDIF} {$ENDIF};

  {**
   *   Opens a Haptic device for usage from a Joystick device.
   *
   *  You must still close the haptic device seperately.  It will not be closed
   *  with the joystick.
   *
   *  When opening from a joystick you should first close the haptic device before
   *  closing the joystick device.  If not, on some implementations the haptic
   *  device will also get unallocated and you'll be unable to use force feedback
   *  on that device.
   *
   *   joystick Joystick to create a haptic device from.
   *   A valid haptic device identifier on success or NULL on error.
   *
   *   SDL_HapticOpen
   *   SDL_HapticClose
   *}
function SDL_HapticOpenFromJoystick(joystick: PSDL_Joystick): PSDL_Haptic cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticOpenFromJoystick' {$ENDIF} {$ENDIF};

  {**
   *   Closes a Haptic device previously opened with SDL_HapticOpen().
   *
   *   haptic Haptic device to close.
   *}
procedure SDL_HapticClose(haptic: PSDL_Haptic) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticClose' {$ENDIF} {$ENDIF};

  {**
   *   Returns the number of effects a haptic device can store.
   *
   *  On some platforms this isn't fully supported, and therefore is an
   *  approximation.  Always check to see if your created effect was actually
   *  created and do not rely solely on SDL_HapticNumEffects().
   *
   *   haptic The haptic device to query effect max.
   *   The number of effects the haptic device can store or
   *   -1 on error.
   *
   *   SDL_HapticNumEffectsPlaying
   *   SDL_HapticQuery
   *}
function SDL_HapticNumEffects(haptic: PSDL_Haptic): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticNumEffects' {$ENDIF} {$ENDIF};

  {**
   *   Returns the number of effects a haptic device can play at the same
   *   time.
   *
   *  This is not supported on all platforms, but will always return a value.
   *  Added here for the sake of completeness.
   *
   *   haptic The haptic device to query maximum playing effects.
   *   The number of effects the haptic device can play at the same time
   *   or -1 on error.
   *
   *   SDL_HapticNumEffects
   *   SDL_HapticQuery
   *}
function SDL_HapticNumEffectsPlaying(haptic: PSDL_Haptic): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticNumEffectsPlaying' {$ENDIF} {$ENDIF};

  {**
   *   Gets the haptic devices supported features in bitwise matter.
   *
   *  Example:
   *
   *  if (SDL_HapticQueryEffects(haptic) & SDL_HAPTIC_CONSTANT)
   *      printf("We have constant haptic effect!");
   *
   *
   *
   *   haptic The haptic device to query.
   *   Haptic features in bitwise manner (OR'd).
   *
   *   SDL_HapticNumEffects
   *   SDL_HapticEffectSupported
   *}
function SDL_HapticQuery(haptic: PSDL_Haptic): UInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticQuery' {$ENDIF} {$ENDIF};

  {**
   *   Gets the number of haptic axes the device has.
   *
   *   SDL_HapticDirection
   *}
function SDL_HapticNumAxes(haptic: PSDL_Haptic): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticNumAxes' {$ENDIF} {$ENDIF};

  {**
   *   Checks to see if effect is supported by haptic.
   *
   *   haptic Haptic device to check on.
   *   effect Effect to check to see if it is supported.
   *   SDL_TRUE if effect is supported, SDL_FALSE if it isn't or -1 on error.
   *
   *   SDL_HapticQuery
   *   SDL_HapticNewEffect
   *}
function SDL_HapticEffectSupported(haptic: PSDL_Haptic; effect: PSDL_HapticEffect): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticEffectSupported' {$ENDIF} {$ENDIF};

  {**
   *   Creates a new haptic effect on the device.
   *
   *   haptic Haptic device to create the effect on.
   *   effect Properties of the effect to create.
   *   The id of the effect on success or -1 on error.
   *
   *   SDL_HapticUpdateEffect
   *   SDL_HapticRunEffect
   *   SDL_HapticDestroyEffect
   *}
function SDL_HapticNewEffect(haptic: PSDL_Haptic; effect: PSDL_HapticEffect): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticNewEffect' {$ENDIF} {$ENDIF};

  {**
   *   Updates the properties of an effect.
   *
   *  Can be used dynamically, although behaviour when dynamically changing
   *  direction may be strange.  Specifically the effect may reupload itself
   *  and start playing from the start.  You cannot change the type either when
   *  running SDL_HapticUpdateEffect().
   *
   *   haptic Haptic device that has the effect.
   *   effect Effect to update.
   *   data New effect properties to use.
   *   The id of the effect on success or -1 on error.
   *
   *   SDL_HapticNewEffect
   *   SDL_HapticRunEffect
   *   SDL_HapticDestroyEffect
   *}
function SDL_HapticUpdateEffect(haptic: PSDL_Haptic; effect: Integer; data: PSDL_HapticEffect): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticUpdateEffect' {$ENDIF} {$ENDIF};

  {**
   *   Runs the haptic effect on its associated haptic device.
   *
   *  If iterations are ::SDL_HAPTIC_INFINITY, it'll run the effect over and over
   *  repeating the envelope (attack and fade) every time.  If you only want the
   *  effect to last forever, set ::SDL_HAPTIC_INFINITY in the effect's length
   *  parameter.
   *
   *   haptic Haptic device to run the effect on.
   *   effect Identifier of the haptic effect to run.
   *   iterations Number of iterations to run the effect. Use
   *   SDL_HAPTIC_INFINITY for infinity.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticStopEffect
   *   SDL_HapticDestroyEffect
   *   SDL_HapticGetEffectStatus
   *}
function SDL_HapticRunEffect(haptic: PSDL_Haptic; effect: Integer; iterations: UInt32): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticRunEffect' {$ENDIF} {$ENDIF};

  {**
   *   Stops the haptic effect on its associated haptic device.
   *
   *   haptic Haptic device to stop the effect on.
   *   effect Identifier of the effect to stop.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticRunEffect
   *   SDL_HapticDestroyEffect
   *}
function SDL_HapticStopEffect(haptic: PSDL_Haptic; effect: Integer): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticStopEffect' {$ENDIF} {$ENDIF};

  {**
   *   Destroys a haptic effect on the device.
   *
   *  This will stop the effect if it's running.  Effects are automatically
   *  destroyed when the device is closed.
   *
   *   haptic Device to destroy the effect on.
   *   effect Identifier of the effect to destroy.
   *
   *   SDL_HapticNewEffect
   *}
procedure SDL_HapticDestroyEffect(haptic: PSDL_Haptic; effect: Integer) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticDestroyEffect' {$ENDIF} {$ENDIF};

  {**
   *   Gets the status of the current effect on the haptic device.
   *
   *  Device must support the ::SDL_HAPTIC_STATUS feature.
   *
   *   haptic Haptic device to query the effect status on.
   *   effect Identifier of the effect to query its status.
   *   0 if it isn't playing, 1 if it is playing or -1 on error.
   *
   *   SDL_HapticRunEffect
   *   SDL_HapticStopEffect
   *}
function SDL_HapticGetEffectStatus(haptic: PSDL_Haptic; effect: Integer): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticGetEffectStatus' {$ENDIF} {$ENDIF};

  {**
   *   Sets the global gain of the device.
   *
   *  Device must support the SDL_HAPTIC_GAIN feature.
   *
   *  The user may specify the maximum gain by setting the environment variable
   *  SDL_HAPTIC_GAIN_MAX which should be between 0 and 100.  All calls to
   *  SDL_HapticSetGain() will scale linearly using SDL_HAPTIC_GAIN_MAX as the
   *  maximum.
   *
   *   haptic Haptic device to set the gain on.
   *   gain Value to set the gain to, should be between 0 and 100.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticQuery
   *}
function SDL_HapticSetGain(haptic: PSDL_Haptic; gain: Integer): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticSetGain' {$ENDIF} {$ENDIF};

  {**
   *   Sets the global autocenter of the device.
   *
   *  Autocenter should be between 0 and 100.  Setting it to 0 will disable
   *  autocentering.
   *
   *  Device must support the ::SDL_HAPTIC_AUTOCENTER feature.
   *
   *   haptic Haptic device to set autocentering on.
   *   autocenter Value to set autocenter to, 0 disables autocentering.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticQuery
   *}
function SDL_HapticSetAutocenter(haptic: PSDL_Haptic; autocenter: Integer): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticSetAutocenter' {$ENDIF} {$ENDIF};

  {**
   *   Pauses a haptic device.
   *
   *  Device must support the SDL_HAPTIC_PAUSE feature.  Call
   *  SDL_HapticUnpause() to resume playback.
   *
   *  Do not modify the effects nor add new ones while the device is paused.
   *  That can cause all sorts of weird errors.
   *
   *   haptic Haptic device to pause.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticUnpause
   *}
function SDL_HapticPause(haptic: PSDL_Haptic): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticPause' {$ENDIF} {$ENDIF};

  {**
   *   Unpauses a haptic device.
   *
   *  Call to unpause after SDL_HapticPause().
   *
   *   haptic Haptic device to pause.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticPause
   *}
function SDL_HapticUnpause(haptic: PSDL_Haptic): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticUnPause' {$ENDIF} {$ENDIF};

  {**
   *   Stops all the currently playing effects on a haptic device.
   *
   *   haptic Haptic device to stop.
   *   0 on success or -1 on error.
   *}
function SDL_HapticStopAll(haptic: PSDL_Haptic): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticStopAll' {$ENDIF} {$ENDIF};

  {**
   *   Checks to see if rumble is supported on a haptic device..
   *
   *   haptic Haptic device to check to see if it supports rumble.
   *   SDL_TRUE if effect is supported, SDL_FALSE if it isn't or -1 on error.
   *
   *   SDL_HapticRumbleInit
   *   SDL_HapticRumblePlay
   *   SDL_HapticRumbleStop
   *}
function SDL_HapticRumbleSupported(haptic: PSDL_Haptic): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticRumbleSupported' {$ENDIF} {$ENDIF};

  {**
   *   Initializes the haptic device for simple rumble playback.
   *
   *   haptic Haptic device to initialize for simple rumble playback.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticOpen
   *   SDL_HapticRumbleSupported
   *   SDL_HapticRumblePlay
   *   SDL_HapticRumbleStop
   *}
function SDL_HapticRumbleInit(haptic: PSDL_Haptic): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticRumbleInit' {$ENDIF} {$ENDIF};

  {**
   *   Runs simple rumble on a haptic device
   *
   *   haptic Haptic device to play rumble effect on.
   *   strength Strength of the rumble to play as a 0-1 float value.
   *   length Length of the rumble to play in milliseconds.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticRumbleSupported
   *   SDL_HapticRumbleInit
   *   SDL_HapticRumbleStop
   *}
function SDL_HapticRumblePlay(haptic: PSDL_Haptic; strength: Float; length: UInt32): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticRumblePlay' {$ENDIF} {$ENDIF};

  {**
   *   Stops the simple rumble on a haptic device.
   *
   *   haptic Haptic to stop the rumble on.
   *   0 on success or -1 on error.
   *
   *   SDL_HapticRumbleSupported
   *   SDL_HapticRumbleInit
   *   SDL_HapticRumblePlay
   *}
function SDL_HapticRumbleStop(haptic: PSDL_Haptic): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HapticRumbleStop' {$ENDIF} {$ENDIF};

