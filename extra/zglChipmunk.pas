{
 * Chipmunk: http://code.google.com/p/chipmunk-physics/
 *
 * Port started by Fernando Nadal and Ruben Javier
 *
 * Fully ported by Paul Robello:
 *   mail:    paulr@par-com.net
 *   version: svn revision 112
 *
 * Port continued by Kemka Andrey
 *   mail:    dr.andru@gmail.com
 *   site:    http://andru-kun.inf.ua
 *   version: svn revision 144
}
unit zglChipmunk;

{.$DEFINE USE_INLINE}
{.$DEFINE CHIPMUNK_DOUBLE_PRECISION} // This is needed when you want to use double precision

interface

uses
  math;

const
CP_CIRCLE_SHAPE  = 0;
CP_SEGMENT_SHAPE = 3;
CP_POLY_SHAPE    = 9;
CP_NUM_SHAPES    = 3;

CP_PIN_JOINT     = 0;
CP_PIVOT_JOINT   = 1;
CP_SLIDE_JOINT   = 2;
CP_GROOVE_JOINT  = 3;
CP_SPRING_JOINT  = 4;
CP_BREAKABLE_JOINT    = 5;
CP_SIMPLE_MOTOR_JOINT = 6;
CP_ROTARY_LIMIT_JOINT = 7;
CP_DAMPED_ROTARY_SPRING_JOINT = 8;
CP_GEAR_JOINT   = 9;
CP_CUSTOM_JOINT = 10;

CP_HASH_COEF = 3344921057;

M_PI = 3.1415926535897932;
M_2PI: single = M_PI * 2;
M_1RAD:single = M_PI / 180;

type
  {simple types}
  {$IFDEF CHIPMUNK_DOUBLE_PRECISION}
   Float = Double;
  {$ELSE}
   Float = Single;
  {$ENDIF}

//const INFINITY = 1e1000;

const
INFINITY: single = MaxSingle;
CP_ARRAY_INCREMENT: integer = 10;

var
cp_bias_coef:single = 0.1;// Determines how fast penetrations resolve themselves.
cp_collision_slop:single = 0.1;// Amount of allowed penetration. Used to reduce vibrating contacts.
cp_constraint_bias_coef:single = 0.1;
cp_contact_persistence: single = 10;// Number of frames that contact information should persist.

(*Comment this line if you get weird errors*)
{$DEFINE NICE_CODE_PARAMS}


///////////////
//Chipmunk types
///////////////


type
  pcpUnsignedIntArray=^cpUnsignedIntArray;
  cpUnsignedIntArray = Array [0..32767] of LongWord;

  PcpFloat = ^cpFloat;
  cpFloat = Float;

  PcpVect = ^cpVect;
  cpVect = record
    x             : cpFloat;
    y             : cpFloat;
  end;
  PcpVectArray = ^cpVectArray;
  cpVectArray = Array [0..32767] of cpVect;


Const cpVZero  : cpVect = (x:  0; y: 0);
      cpVRight : cpVect = (x:  1; y: 0);
      cpVLeft  : cpVect = (x: -1; y: 0);
      cpVUp    : cpVect = (x:  0; y: 1);
      cpVDown  : cpVect = (x:  0; y:-1);

Type
  PcpBB = ^cpBB;
  cpBB = record
    l             : cpFloat;
    b             : cpFloat;
    r             : cpFloat;
    t             : cpFloat;
  end;


  PcpBody = ^cpBody;

  cpBodyVelocityFunc = procedure(cpBody:PcpBody; const gravity:cpVect; const damping,dt:cpFloat);
  cpBodyPositionFunc = procedure(cpBody:PcpBody; const dt:cpFloat);


  // NOTE: cpArray is rarely used and will probably go away.
  PcpPointerArray=^cpPointerArray;
  cpPointerArray=Array [0..32767] of pointer;

  PcpArray = ^cpArray;
  cpArray = record
    num           : integer; // current number used
    max           : integer; // current number alocated
    arr           : PcpPointerArray; // actual data
  end;
  PcpArrayIter = ^TcpArrayIter;
  TcpArrayIter = procedure (ptr : pointer; data : pointer ); //CFUNCTYPE(None, c_void_p, c_void_p) // typedef void (*cpArrayIter)(void *ptr, void *data);

  PcpSpace = ^cpSpace;

  cpBody = record
    velocity_func : cpBodyVelocityFunc;
    position_func : cpBodyPositionFunc;
    m             : cpFloat;// Mass and it's inverse.
    m_inv         : cpFloat;
    i             : cpFloat;// Moment of inertia and it's inverse.
    i_inv         : cpFloat;
    p             : cpVect;// Linear components of motion (position, velocity, and force)
    v             : cpVect;
    f             : cpVect;
    a             : cpFloat;// Angular components of motion (angle, angular velocity, and torque)
    w             : cpFloat;
    t             : cpFloat;
    rot           : cpVect;// Unit length
    data          : pointer; // user defined data
    tag           : cardinal; // user usable field
    v_bias        : cpVect;// NOTE: v_bias and w_bias are used internally for penetration/constraint correction.
    w_bias        : cpFloat;// NOTE: v_bias and w_bias are used internally for penetration/constraint correction.
    shapes        : PcpArray; // all shapes using this body
    space         : PcpSpace; // space body belongs to
  end;
  pcpBodyPair=^cpBodyPair;
  cpBodyPair=array[0..1] of pcpbody;


  // cpHashSet uses a chained hashtable implementation.
  // Other than the transformation functions, there is nothing fancy going on.

  // cpHashSetBin's form the linked lists in the chained hash table.
  PcpHashSetBin = ^cpHashSetBin;
  PPcpHashSetBin = ^PcpHashSetBin;
  cpHashSetBin = record
    elt           : pointer;// Pointer to the element.
    hash          : LongWord;// Hash value of the element.
    next          : PcpHashSetBin;// Next element in the chain.
  end;
  PcpHashSetBinArray = ^cpHashSetBinArray;
  cpHashSetBinArray = Array [0..32767] of PcpHashSetBin;


  // Equality function. Returns true if ptr is equal to elt.
  TcpHashSetEqlFunc = function (ptr : pointer; elt : pointer): boolean;

  // Used by cpHashSetInsert(). Called to transform the ptr into an element.
  TcpHashSetTransFunc = function (ptr : pointer; data : pointer): pointer;

  // Iterator function for a hashset.
  TcpHashSetIterFunc = procedure (elt : pointer; data : pointer);

  // Reject function. Returns true if elt should be dropped.
  TcpHashSetRejectFunc = function (elt : pointer; data : pointer): integer;

  PcpHashSet = ^cpHashSet;
  cpHashSet = record
    entries       : integer;// Number of elements stored in the table.
    size          : integer;// Number of cells in the table.
    eql           : TcpHashSetEqlFunc;
    trans         : TcpHashSetTransFunc;
    default_value : pointer;// Default value returned by cpHashSetFind() when no element is found.// Defaults to NULL.
    table         : PcpHashSetBinArray;
  end;

  // The spatial hash is Chipmunk's default (and currently only) spatial index type.
  // Based on a chained hash table.
  // Used internally to track objects added to the hash
  PcpHandle = ^cpHandle;
  cpHandle = record
    obj           : pointer;// Pointer to the object
    retain        : integer;// Retain count
    stamp         : integer;// Query stamp. Used to make sure two objects
  end;                  // aren't identified twice in the same query.

  // Linked list element for in the chains.
  PcpSpaceHashBin = ^cpSpaceHashBin;
  cpSpaceHashBin = record
    handle        : PcpHandle;
    next          : PcpSpaceHashBin;
  end;
  PcpSpaceHashBinArray=^cpSpaceHashBinArray;
  cpSpaceHashBinArray = Array [0..32767] of pcpSpaceHashBin;

  // BBox callback. Called whenever the hash needs a bounding box from an object.
  PTcpSpaceHashBBFunc = ^TcpSpaceHashBBFunc;
  TcpSpaceHashBBFunc = function (obj : pointer): cpBB;

  PcpSpaceHash = ^cpSpaceHash;
  cpSpaceHash = record
    numcells      : integer;// Number of cells in the table.
    celldim       : cpFloat;// Dimentions of the cells.
    bbfunc        : TcpSpaceHashBBFunc;// BBox callback.
    handleSet     : PcpHashSet;// Hashset of all the handles.
    table         : PcpSpaceHashBinArray;
    bins          : PcpSpaceHashBin;// List of recycled bins.
    stamp         : integer;// Incremented on each query. See cpHandle.stamp.
  end;

  // Iterator function
  TcpSpaceHashIterator = procedure (obj : pointer; data : pointer); //maybe a procedure

  // Query callback.
  TcpSpaceHashQueryFunc = procedure (obj1,obj2,data : pointer);

   PcpEachPair=^cpEachPair;
   cpEachPair=record
      func:TcpSpaceHashIterator;
      data:pointer;
   end;

   // Similar to struct eachPair above.
   pcpQueryRehashPair =^cpQueryRehashPair;
   cpQueryRehashPair = record
      hash:pcpSpaceHash;
      func:TcpSpaceHashQueryFunc;
      data:pointer;
   end;

// Enumeration of shape types.
  cpShapeType = integer;

  // Basic shape struct that the others inherit from.
  PcpShape = ^cpShape;
  PcpShapeClass = ^cpShapeClass;
  cpShapeClass = record
    cptype        : cpShapeType;//original name was "type"
    cacheData     : function (shape : PcpShape; const p,rot : cpVect) : cpBB; //Called by cpShapeCacheBB().
    destroy       : procedure (shape : PcpShape); // Called to by cpShapeDestroy().
    pointQuery    : function (shape : PcpShape; const p : cpVect) : integer; // called by cpShapeQueryPointQuery()
  end;

  cpShape = record
    klass         : PcpShapeClass;
    space         : PcpSpace;
    body          : PcpBody;// cpBody that the shape is attached to.
    bb            : cpBB;// Cached BBox for the shape.
    e             : cpFloat;// Coefficient of restitution. (elasticity)
    u             : cpFloat;// Coefficient of friction.
    surface_v     : cpVect;// Surface velocity used when solving for friction.
    data          : pointer;// User defined data pointer for the shape.
    collision_type: LongWord;// User defined collision type for the shape.
    group         : LongWord;// User defined collision group for the shape.
    layers        : LongWord;// User defined layer bitmask for the shape.
    is_static     : boolean;// set to true if body is static
    id            : LongWord;// Unique id used as the hash value.
    tag           : LongWord;// user usable
    offset        : cpVect; // used to store offset of obj for use with static bodies
  end;
  cpShapePair=array[0..1] of pcpShape;
  cpShapeArray=Array [0..32767] of PcpShape;
  PcpShapeArray=^cpShapeArray;


  // Circle shape structure.
  PcpCircleShape = ^cpCircleShape;
  cpCircleShape = record
    shape         : cpShape;
    c             : cpVect;// Center in body space coordinates
    r             : cpFloat;// Radius.
    tc            : cpVect;// Transformed center. (world space coordinates)
  end;

  // Segment shape structure.
  PcpSegmentShape = ^cpSegmentShape;
  cpSegmentShape = record
    shape         : cpShape;
    a             : cpVect;// Endpoints and normal of the segment.a,b,n (body space coordinates)
    b             : cpVect;
    n             : cpVect;
    r             : cpFloat;// Radius of the segment. (Thickness)
    ta            : cpVect;// Transformed endpoints and normal.ta,tb,tn (world space coordinates)
    tb            : cpVect;
    tn            : cpVect;
  end;

  // Axis structure used by cpPolyShape.
  PcpPolyShapeAxis = ^cpPolyShapeAxis;
  cpPolyShapeAxis = record
    n             : cpVect;// normal
    d             : cpFloat;// distance from origin
  end;

  cpPolyShapeAxisArray = Array [0..32767] of cpPolyShapeAxis;
  PcpPolyShapeAxisArray = ^cpPolyShapeAxisArray;

  // Convex polygon shape structure.
  PcpPolyShape = ^cpPolyShape;
  cpPolyShape = record
    shape         : cpShape;
    numVerts      : integer;// Vertex and axis list count.
    verts         : PcpVectArray;
    axes          : PcpPolyShapeAxisArray;
    tVerts        : PcpVectArray;// Transformed vertex and axis lists.
    tAxes         : PcpPolyShapeAxisArray;
    convex        : boolean; // true if poly is convex
  end;

  // Data structure for contact points.
  PcpContact = ^cpContact;
  cpContact = record
    p             : cpVect;// Contact point.
    n             : cpVect;// Contact point normal.
    dist          : cpFloat;// Penetration distance.
    r1            : cpVect;// Calculated by cpArbiterPreStep.
    r2            : cpVect;// Calculated by cpArbiterPreStep.
    nMass         : cpfloat;// Calculated by cpArbiterPreStep.
    tMass         : cpfloat;// Calculated by cpArbiterPreStep.
    bounce        : cpfloat;// Calculated by cpArbiterPreStep.
    jnAcc         : cpfloat;// Persistant contact information.
    jtAcc         : cpfloat;// Persistant contact information.
    jBias         : cpfloat;// Persistant contact information.
    bias          : cpfloat;// Persistant contact information.
    hash          : LongWord;// Hash value used to (mostly) uniquely identify a contact.
  end;
  cpContactArray = Array [0..32767] of cpContact;
  PcpContactArray = ^cpContactArray;

  // Data structure for tracking collisions between shapes.
  PcpArbiter = ^cpArbiter;
  cpArbiter = record
    numContacts   : integer;// Information on the contact points between the objects.
    contacts      : PcpContactArray;// Information on the contact points between the objects.
    a             : PcpShape;// The two shapes involved in the collision.
    b             : PcpShape;// The two shapes involved in the collision.
    u             : cpFloat;// Calculated by cpArbiterPreStep().
    target_v      : cpVect;// Calculated by cpArbiterPreStep().
    stamp         : integer;// Time stamp of the arbiter. (from cpSpace)
  end;

  PcpConstraintClass = ^cpConstraintClass;
  PcpConstraint = ^cpConstraint;

  cpConstraintPreStepFunction = procedure(constraint : PcpConstraint; const dt,dt_inv : cpFloat);
  cpConstraintApplyImpulseFunction = procedure(constraint : PcpConstraint);
  cpConstraintGetImpulseFunction = function(constraint : PcpConstraint) : cpFloat;

  cpConstraintClass = record
    cpType        : integer;
    preStep       : cpConstraintPreStepFunction;
    applyImpulse  : cpConstraintApplyImpulseFunction;
    getImpulse    : cpConstraintGetImpulseFunction;
  end;

  cpConstraint = record
    klass         : PcpConstraintClass;
    a             : PcpBody; // bodies attached to constraint
    b             : PcpBody;
    maxForce      : cpFloat; // max force before constraint breaks =0 never
    biasCoef      : cpFloat;
    maxBias       : cpFloat;
    space         : pcpSpace;
  end;

  PcpBreakableJoint=^cpBreakableJoint;
  cpBreakableJoint = record
     constraint    : cpConstraint;
     delegate      : PcpConstraint;
     last_dt_inv   : cpFloat;
     free_delegate : boolean;
  end;

  PcpSimpleMotor = ^cpSimpleMotor;
  cpSimpleMotor = record
     constraint    : cpConstraint;
     iSum          : cpFloat;
     rate          : cpFloat;
     jAcc          : cpFloat;
     jMax          : cpFloat;
  end;

  PcpGearJoint=^cpGearJoint;
  cpGearJoint = record
     constraint    : cpConstraint;
     iSum          : cpFloat;
     phase         : cpFloat;
     ratio         : cpFloat;
     bias          : cpFloat;
     jAcc          : cpFloat;
     jMax          : cpFloat;
  end;

  PcpDampedRotarySpring = ^cpDampedRotarySpring;
  cpDampedRotarySpring = record
     constraint    : cpConstraint;
     restAngle     : cpFloat;
     stiffness     : cpFloat;
     damping       : cpFloat;
     dt            : cpFloat;
     target_wrn    : cpFloat;
     iSum          : cpFloat;
  end;

  PcpRotaryLimitJoint = ^cpRotaryLimitJoint;
  cpRotaryLimitJoint = record
     constraint    : cpConstraint;
     iSum          : cpFloat;
     Min           : cpFloat;
     Max           : cpFloat;
     bias          : cpFloat;
     jAcc          : cpFloat;
     jMax          : cpFloat;
  end;

  PcpDampedSpring = ^cpDampedSpring;
  cpDampedSpring = record
     constraint    : cpConstraint;
     anchr1        : cpVect;
     anchr2        : cpVect;
     restLength    : cpFloat;
     stiffness     : cpFloat;
     damping       : cpFloat;
     dt            : cpFloat;
     target_vrn    : cpFloat;
     r1            : cpVect;
     r2            : cpVect;
     nMass         : cpFloat;
     f_spring      : cpFloat;
     n             : cpVect;
  end;

  PcpPinJoint = ^cpPinJoint;
  cpPinJoint = record
    constraint    : cpConstraint;
    anchr1        : cpVect;
    anchr2        : cpVect;
    dist          : cpFloat;
    r1            : cpVect;
    r2            : cpVect;
    n             : cpVect;
    nMass         : cpFloat;
    jnAcc         : cpFloat;
    jnMax         : cpFloat;
    bias          : cpFloat;
  end;

  PcpSlideJoint = ^cpSlideJoint;
  cpSlideJoint = record
    constraint    : cpConstraint;
    anchr1        : cpVect;
    anchr2        : cpVect;
    min           : cpFloat;
    max           : cpFloat;
    r1            : cpVect;
    r2            : cpVect;
    n             : cpVect;
    nMass         : cpFloat;
    jnAcc         : cpFloat;
    jnMax         : cpFloat;
    bias          : cpFloat;
  end;

  PcpPivotJoint = ^cpPivotJoint;
  cpPivotJoint = record
    constraint    : cpConstraint;
    anchr1        : cpVect;
    anchr2        : cpVect;
    r1            : cpVect;
    r2            : cpVect;
    k1            : cpVect;
    k2            : cpVect;
    jAcc          : cpVect;
    jMaxLen       : cpFloat;
    bias          : cpVect;
  end;

  PcpGrooveJoint = ^cpGrooveJoint;
  cpGrooveJoint = record
    constraint    : cpConstraint;
    grv_n         : cpVect;
    grv_a         : cpVect;
    grv_b         : cpVect;
    anchr2        : cpVect;
    grv_tn        : cpVect;
    clamp         : cpFloat;
    r1            : cpVect;
    r2            : cpVect;
    k1            : cpVect;
    k2            : cpVect;
    jAcc          : cpVect;
    jMaxLen       : cpFloat;
    bias          : cpVect;
  end;

  // User collision pair function.
  TcpCollFunc = function (a,b : PcpShape; contacts : PcpContactArray; numContacts : integer; normal_coef : cpFloat; data : pointer): integer;

  // Structure for holding collision pair function information.
  // Used internally.
  PcpCollPairFunc = ^cpCollPairFunc;
  cpCollPairFunc = record
    a             : LongWord;
    b             : LongWord;
    func          : TcpCollFunc;
    data          : pointer;
  end;

  pcpCollFuncData=^cpCollFuncData;
  cpCollFuncData = record
     func :TcpCollFunc;
     data:pointer;
  end;

  cpSpacePointQueryFunc = procedure(shape : PcpShape; data : pointer);

  PcpPointQueryFuncPair=^TcpPointQueryFuncPair;
  TcpPointQueryFuncPair = record
     func : cpSpacePointQueryFunc;
     data : pointer;
  end;

  cpSpace = record
    iterations    : integer;// Number of iterations to use in the impulse solver.
    elasticIterations : integer;  // Number of iterations to use in the impulse solver to solve elastic collisions.
    gravity       : cpVect;// Self explanatory.
    damping       : cpFloat;// Self explanatory.
    stamp         : integer;// Time stamp. Is incremented on every call to cpSpaceStep().
    staticShapes  : PcpSpaceHash;// The static and active shape spatial hashes.
    activeShapes  : PcpSpaceHash;// The static and active shape spatial hashes.
    bodies        : PcpArray;// List of bodies in the system.
    arbiters      : PcpArray;// List of active arbiters for the impulse solver.
    contactSet    : PcpHashSet;// Persistant contact set.
    constraints   : PcpArray;// List of constraints in the system.
    collFuncSet   : PcpHashSet;// Set of collisionpair functions.
    defaultPairFunc : cpCollPairFunc;// Default collision pair function.
  end;

  // Iterator function for iterating the bodies in a space.
  TcpSpaceBodyIterator = procedure (body : PcpBody; data : pointer); //maybe a procedure

  TcpCollisionFunc=function (a,b:pcpShape; var contact:PcpContactArray):integer;
  PcpCollisionFuncArray = ^TcpCollisionFuncArray;
  TcpCollisionFuncArray = Array [0..32767] of TcpCollisionFunc;


// *****************************************************************************************************************************
//
// main functions
//
// *****************************************************************************************************************************
function calloc(Num,ElemSize : integer) : pointer; overload;{$IFDEF USE_INLINE}inline;{$ENDIF}

function calloc(Size : integer) : pointer; overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
procedure cfree(p : pointer); {$IFDEF USE_INLINE}inline;{$ENDIF}
function malloc(Num,ElemSize : integer) : pointer; overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
function malloc(const Size : integer) : pointer; overload;{$IFDEF USE_INLINE}inline;{$ENDIF}


// DO NOT ADD TO THE PARMS ON THESE THE COMPILER WILL BLOW UP!
function CP_HASH_PAIR(A, B :LongWord):LongWord; overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
function CP_HASH_PAIR(A, B :pointer):LongWord; overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
function CP_HASH_PAIR(A: Longword; B :pointer):LongWord; overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
function CP_HASH_PAIR(A: Pointer; B :Longword):LongWord; overload;{$IFDEF USE_INLINE}inline;{$ENDIF}

procedure cpInitChipmunk;
procedure cpShutdownChipmunk;
procedure cpAddColFunc(a, b:cpShapeType; func:TCPCollisionFunc);
procedure cpInitCollisionFuncs;
function cpMomentForPoly(const m : cpFloat; const numVerts : integer; verts : PcpVectArray; const offset : cpVect) : cpFloat;
function cpPolyArea(verts : pcpVectArray; const numVerts : integer) : cpFloat;
function cpPolyMass(verts : pcpVectArray; const numVerts : integer; const Density : cpFloat) : cpFloat; {$IFDEF USE_INLINE}inline;{$ENDIF}

function cpMomentForCircle(const m,r1,r2 : cpFloat; const offset : cpVect) : cpFloat; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpCircleArea(const r : cpFloat) : cpFloat;{$IFDEF USE_INLINE}inline;{$ENDIF}
function cpCircleMass(const r,Density : cpFloat) : cpFloat;{$IFDEF USE_INLINE}inline;{$ENDIF}

function cpMomentForSegment(const m:cpFloat; const a,b:cpVect) : cpFloat;{$IFDEF USE_INLINE}inline;{$ENDIF}




// math functions
function cpfmax(const a,b : cpFloat) : cpFloat; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpfmin(const a,b : cpFloat) : cpFloat; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpfclamp(const f,min,max : cpFloat) : cpFloat; {$IFDEF USE_INLINE}inline;{$ENDIF}

// *****************************************************************************************************************************
//
// Vect functions
//
// *****************************************************************************************************************************

function cpv(const x,y : cpFloat):cpVect; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvadd(const v1,v2 : cpVect):cpVect; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvneg(const v : cpVect):cpVect; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvsub(const v1,v2 : cpVect):cpVect; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvmult(const v : cpVect; const s : cpFloat):cpVect; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvdot(const v1,v2 : cpVect):cpFloat; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvcross(const v1,v2 : cpVect):cpFloat; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvperp(const v : cpVect):cpVect; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvrperp(const v : cpVect):cpVect; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvproject(const v1,v2 : cpVect):cpVect; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvlerp(const v1,v2 : cpVect; const dt : cpFloat):cpVect;{$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvrotate(const v1,v2 : cpVect):cpVect; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvunrotate(const v1,v2 : cpVect):cpVect; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvforangle(const a : cpFloat) : cpVect; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvtoangle(const v : cpVect) : cpFloat; //{$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvlength(const v : cpVect) : cpFloat; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvdist(const v1,v2 : cpVect) : cpFloat; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvdistsq(const v1,v2 : cpVect) : cpFloat; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvlengthsq(const v : cpVect) : cpFloat; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvnormalize(const v : cpVect) : cpVect; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvstr(const v : cpVect) : string; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpvEqual(const v1,v2 : cpVect) : boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
function modf(const a,b: cpFloat) : cpFloat; {$IFDEF USE_INLINE}inline;{$ENDIF}

// *****************************************************************************************************************************
//
// BB functions
//
// *****************************************************************************************************************************

function cpBBNew (const l,b,r,t : cpFloat):cpBB; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpBBintersects(const a,b : cpBB):integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpBBcontainsBB(const bb,other : cpBB):integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpBBcontainsVect(const bb : cpBB; const v : cpVect):integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
function  cpBBClampVect(const bb : cpBB; const v : cpVect) : cpVect; {$IFDEF USE_INLINE}inline;{$ENDIF}
function  cpBBWrapVect(const bb : cpBB; const v : cpVect) : cpVect; {$IFDEF USE_INLINE}inline;{$ENDIF}

// *****************************************************************************************************************************
//
// Body functions
//
// *****************************************************************************************************************************

// Basic allocation/destruction functions
function  cpBodyAlloc() : PcpBody;
function cpBodyInit( body : PcpBody; const m,i : cpFloat) : PcpBody;
function  cpBodyNew( const m,i : cpFloat) : PcpBody;
procedure  cpBodyDestroy( body : PcpBody);
procedure  cpBodyFree( body : PcpBody);
// Setters for some of the special properties (mandatory!)
procedure  cpBodySetMass( body : PcpBody; const m : cpFloat);
procedure  cpBodySetMoment( body : PcpBody; const i : cpFloat);
procedure cpBodySetAngle( body : PcpBody; const a : cpFloat);

// Modify the velocity of an object so that it will
procedure  cpBodySlew( body : PcpBody; const pos : cpVect; const dt : cpFloat);
// Integration functions.
procedure  cpBodyUpdateVelocity( body : PcpBody; const gravity : cpVect; const damping,dt : cpFloat);
procedure  cpBodyUpdatePosition( body : PcpBody; const dt : cpFloat);

// Convert body local to world coordinates
function cpBodyLocal2World( body : PcpBody; const v : cpVect):cpVect;
// Convert world to body local coordinates
function cpBodyWorld2Local( body : PcpBody; const v : cpVect):cpVect;
// Apply an impulse (in world coordinates) to the body.
procedure cpBodyApplyImpulse( body : PcpBody; const j,r : cpVect);
// Not intended for external use. Used by cpArbiter.c and cpJoint.c.
procedure cpBodyApplyBiasImpulse( body : PcpBody; const j,r : cpVect);

// Zero the forces on a body.
procedure  cpBodyResetForces( body : PcpBody); {$IFDEF USE_INLINE}inline;{$ENDIF}
// Apply a force (in world coordinates) to a body.
procedure  cpBodyApplyForce( body : PcpBody; const f,r : cpVect);
// Apply a damped spring force between two bodies.
procedure cpApplyDampedSpring( a,b : PcpBody; const anchr1,anchr2 : cpVect; const rlen, k, dmp, dt : cpFloat);

//distance between two bodies
function cpvBodyDist(b1,b2 : pcpBody) : cpFloat; {$IFDEF USE_INLINE}inline;{$ENDIF}

procedure cpResetForcesAllBodies( Space :pcpSpace );


// *****************************************************************************************************************************
//
// Array functions
//
// *****************************************************************************************************************************


// NOTE: cpArray is rarely used and will probably go away.
function  cpArrayAlloc() : PcpArray;
function  cpArrayInit( arr : PcpArray; size :integer) : PcpArray;
function  cpArrayNew( const size :integer) : PcpArray;
procedure  cpArrayDestroy( arr : PcpArray );
procedure  cpArrayFree( arr : PcpArray );
procedure  cpArrayPush( arr : PcpArray; cpobject : pointer );
procedure  cpArrayDeleteIndex( arr : PcpArray; const index :integer);
procedure  cpArrayDeleteObj( arr : PcpArray; obj : pointer );
procedure  cpArrayEach( arr : PcpArray; iterFunc : TcpArrayIter; data : pointer );
function  cpArrayContains( arr : PcpArray; ptr : pointer ) : integer;

// *****************************************************************************************************************************
//
// HashSet functions
//
// *****************************************************************************************************************************

// cpHashSet uses a chained hashtable implementation.
// Other than the transformation functions, there is nothing fancy going on.

// Basic allocation/destruction functions.
function cpHashSetAlloc : PcpHashSet;
function cpHashSetInit( cpset : PcpHashSet; size : integer; eqlFunc : TcpHashSetEqlFunc; trans : TcpHashSetTransFunc) : PcpHashSet;
function cpHashSetNew( const size : integer; eqlFunc : TcpHashSetEqlFunc; trans : TcpHashSetTransFunc) : PcpHashSet;
procedure cpHashSetDestroy( cpset : PcpHashSet);
procedure cpHashSetFree( cpset : PcpHashSet);
function cpHashSetIsFull( cpset : PcpHashSet) : boolean;
procedure cpHashSetResize( cpset : PcpHashSet );

// Insert an element into the set, returns the element.
// If it doesn't already exist, the transformation function is applied.
function  cpHashSetInsert( cpset : PcpHashSet; hash : LongWord; ptr : pointer; data : pointer) : pointer;
// Remove and return an element from the set.
function  cpHashSetRemove( cpset : PcpHashSet; hash : LongWord; ptr : pointer) : pointer;
// Find an element in the set. Returns the default value if the element isn't found.
function  cpHashSetFind( cpset : PcpHashSet; hash : LongWord; ptr : pointer) : pointer;

// Iterate over a hashset.
procedure  cpHashSetEach( cpset : PcpHashSet; func : TcpHashSetIterFunc; data : pointer);
// Iterate over a hashset while rejecting certain elements.
procedure  cpHashSetReject( cpset : PcpHashSet; func : TcpHashSetRejectFunc; data : pointer);

// *****************************************************************************************************************************
//
// SpaceHash functions
//
// *****************************************************************************************************************************

//Basic allocation/destruction functions.
function cpHandleAlloc:pcpHandle;
function cpHandleInit(hand:pcpHandle; obj:pointer):pcpHandle;
function cpHandleNew(obj:pointer):pcpHandle;
procedure cphandleFreeWrap(elt:pointer; unused:pointer);
procedure cpHandleRetain(hand:pcpHandle);  {$IFDEF USE_INLINE}inline;{$ENDIF}
procedure cpHandleFree(hand:pcpHandle);  {$IFDEF USE_INLINE}inline;{$ENDIF}
procedure cpHandleRelease(hand:pcpHandle);  {$IFDEF USE_INLINE}inline;{$ENDIF}
procedure cpClearHashCell(hash:pcpSpaceHash; index:integer);  {$IFDEF USE_INLINE}inline;{$ENDIF}
procedure cpfreeBins(hash:pcpSpaceHash);
procedure cpClearHash(hash:pcpSpaceHash);
procedure cpSpaceHashAllocTable(hash:pcpSpaceHash; numcells: integer);
function  cpSpaceHashAlloc() : PcpSpaceHash;
function  cpSpaceHashInit( hash : PcpSpaceHash; celldim : cpFloat; numcells : integer; bbfunc : TcpSpaceHashBBFunc) : PcpSpaceHash;
function  cpSpaceHashNew( celldim : cpFloat; cells : integer; bbfunc : TcpSpaceHashBBFunc) : PcpSpaceHash;

procedure cpSpaceHashDestroy( hash : PcpSpaceHash);
procedure cpSpaceHashFree( hash : PcpSpaceHash);
function cpContainsHandle(bin:pcpSpaceHashBin; hand:pcpHandle):integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
function getEmptyBin(hash:pcpSpaceHash) :pcpSpaceHashBin; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cphash_func(x,y,n:LongWord) :LongWord; {$IFDEF USE_INLINE}inline;{$ENDIF}
procedure cphashHandle(hash:pcpSpaceHash; hand:pcpHandle; bb:cpBB); {$IFDEF USE_INLINE}inline;{$ENDIF}

// Resize the hashtable. (Does not rehash! You must call cpSpaceHashRehash() if needed.)
procedure cpSpaceHashResize( hash : PcpSpaceHash; const celldim : cpFloat; const numcells: Integer);
// Add an object to the hash.
procedure cpSpaceHashInsert( hash : PcpSpaceHash; obj : pointer; id : LongWord; bb : cpBB);
// Remove an object from the hash.
procedure cpSpaceHashRemove( hash : PcpSpaceHash; obj : pointer; id : LongWord);
// Iterate over the objects in the hash.
procedure cpSpaceHashEach( hash : PcpSpaceHash; func : TcpSpaceHashIterator; data : pointer);
// Rehash the contents of the hash.
procedure cpSpaceHashRehash( hash : PcpSpaceHash);
// Rehash only a specific object.
procedure cpSpaceHashRehashObject( hash : PcpSpaceHash; obj : pointer; id : LongWord);
// Query the hash for a given BBox.
procedure cpSpaceHashQuery( hash : PcpSpaceHash; obj : pointer; const bb : cpBB; func : TcpSpaceHashQueryFunc; data : pointer);
// Rehashes while querying for each object. (Optimized case)
procedure cpSpaceHashQueryRehash( hash : PcpSpaceHash; func : TcpSpaceHashQueryFunc; data : pointer);

// *****************************************************************************************************************************
//
// Shape functions
//
// *****************************************************************************************************************************

// For determinism, you can reset the shape id counter.
procedure  cpResetShapeIdCounter;

// Low level shape initialization func.
function cpShapeInit( shape : PcpShape; klass:PcpShapeClass; body : PcpBody) : PcpShape;

// Basic destructor functions. (allocation functions are not shared)
procedure cpShapeDestroy( shape : PcpShape);
procedure cpShapeFree( shape : PcpShape);

// Cache the BBox of the shape.
function cpShapeCacheBB( shape : PcpShape) : cpBB;  {$IFDEF USE_INLINE}inline;{$ENDIF}
function bbFromCircle(const c:cpVect; const r:cpFloat ) :cpBB; {$IFDEF USE_INLINE}inline;{$ENDIF}

// Basic allocation functions for cpCircleShape.
function cpCircleShapeCacheData(shape : PcpShape; const p,rot : cpVect):cpBB;
function cpCircleShapeAlloc() : PcpCircleShape;
function cpCircleShapeInit( circle : PcpCircleShape; body : PcpBody; radius : cpFloat; offset : cpVect) : PcpCircleShape;
function cpCircleShapeNew( body : PcpBody; radius : cpFloat; offset : cpVect) : PcpShape;
function cpCircleShapePointQuery(shape:PcpShape; const p:cpVect) : integer;

// Basic allocation functions for cpSegmentShape.
function cpSegmentShapeCacheData(shape:pcpShape; const p,rot:cpVect):cpBB;
function cpSegmentShapeAlloc() : PcpSegmentShape;
function cpSegmentShapeInit( seg : PcpSegmentShape; body : PcpBody; a,b : cpVect; r : cpFloat) : PcpSegmentShape;
function cpSegmentShapeNew( body : PcpBody; a,b : cpVect; r:cpFloat) : PcpShape;
function cpSegmentShapePointQuery(shape:PcpShape; const p:cpVect) : integer;

// Unsafe API
procedure cpCircleShapeSetRadius( shape: PcpShape; radius: Float );
procedure cpCircleShapeSetCenter( shape: PcpShape; center: cpVect );
procedure cpSegmentShapeSetEndpoints( shape: PcpShape; a,b : cpVect );
procedure cpSegmentShapeSetRadius( shape: PcpShape; radius: Float );

// *****************************************************************************************************************************
//
// PolyShape functions
//
// *****************************************************************************************************************************

// Basic allocation functions.
procedure cpPolyShapeTransformAxes(poly:pcpPolyShape; p:cpVect; rot:cpVect);
procedure cpPolyShapeTransformVerts(poly:pcpPolyShape; p,rot:cpVect);
function cpPolyShapeCacheData(shape:pcpShape; const p,rot:cpVect) :cpBB;
function cpIsPolyConvex(verts : pcpVectArray; numVerts : integer) : boolean;

procedure cpPolyShapeDestroy(shape:pcpShape);
function cpPolyShapeAlloc : PcpPolyShape;
function cpPolyShapeInit( poly : PcpPolyShape; body : PcpBody; numVerts : integer; verts : PcpVectArray; offset : cpVect; assumeConvex : boolean = true) : PcpPolyShape;
function cpPolyShapeNew( body : PcpBody; numVerts : integer; verts : PcpVectArray; offset : cpVect) : PcpShape;
procedure cpPolyShapeSetVerts( shape: PcpShape; numVerts: Integer; verts: PcpVectArray; offset: cpVect );
function cpPolyShapePointQuery(shape:PcpShape; const p:cpVect) : integer;

// Returns the minimum distance of the polygon to the axis.
function cpPolyShapeValueOnAxis( poly : PcpPolyShape; n : cpVect; d : cpFloat) : cpFloat; {$IFDEF USE_INLINE}inline;{$ENDIF}
// Returns true if the polygon contains the vertex.
function cpPolyShapeContainsVert( poly : PcpPolyShape; v : cpVect) : integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
function cpPolyShapeContainsVertPartial(poly : PcpPolyShape; v,n : cpVect) : integer; {$IFDEF USE_INLINE}inline;{$ENDIF}

// *****************************************************************************************************************************
//
// Arbiter functions
//
// *****************************************************************************************************************************

// Contacts are always allocated in groups.
function cpContactInit( con : PcpContact; p,n : cpVect; dist : cpFloat; hash : LongWord ) : PcpContact;
// Sum the contact impulses. (Can be used after cpSpaceStep() returns)
function cpContactsSumImpulses( contacts : PcpContactArray; numContacts :integer) : cpVect;
function cpContactsSumImpulsesWithFriction( contacts : PcpContactArray; numContacts :integer) : cpVect;

// Basic allocation/destruction functions.
function cpArbiterAlloc() : PcpArbiter;
function cpArbiterInit( arb : PcpArbiter; a,b : PcpShape; stamp :integer) : PcpArbiter;
function cpArbiterNew( a,b : PcpShape; stamp :integer) : PcpArbiter;
procedure cpArbiterDestroy( arb : PcpArbiter );
procedure cpArbiterFree( arb : PcpArbiter );
procedure cpFreeArbiters( space : pcpspace);

// These functions are all intended to be used internally.
// Inject new contact points into the arbiter while preserving contact history.
procedure cpArbiterInject( arb : PcpArbiter; var contacts : PcpContactArray; numContacts :integer);
// Precalculate values used by the solver.
procedure cpArbiterPreStep( arb : PcpArbiter; dt_inv : cpFloat );
// Run an iteration of the solver on the arbiter.
procedure cpArbiterApplyImpulse( arb : PcpArbiter; eCoef : cpFloat );

// *****************************************************************************************************************************
//
// Collision functions
//
// *****************************************************************************************************************************

// Collides two cpShape structures. (this function is lonely :( )
function cpAddContactPoint(var arr:PcpContactArray; var max, num:integer) :pcpContact;
function  cpCollideShapes( a,b : PcpShape; var arr : PcpContactArray) : integer;

// *****************************************************************************************************************************
//
// constraint functions
//
// *****************************************************************************************************************************

procedure cpConstraintDestroy( constraint : PcpConstraint);
procedure cpConstraintFree( constraint : PcpConstraint);
procedure cpConstraintInit(constraint : PcpConstraint; klass: PcpConstraintClass; a,b: PcpBody);

function relative_velocity(a,b: PcpBody; r1,r2:cpVect) :cpVect; {$IFDEF USE_INLINE}inline;{$ENDIF}
function normal_relative_velocity(a,b: PcpBody; r1,r2,n:cpVect) :cpFloat; {$IFDEF USE_INLINE}inline;{$ENDIF}
function mult_k(const vr,k1,k2: cpVect) : cpVect;{$IFDEF USE_INLINE}inline;{$ENDIF}
function clamp_vect(const v: cpVect; const len : cpFloat) : cpVect;{$IFDEF USE_INLINE}inline;{$ENDIF}


function k_scalar(a,b: PcpBody; const r1,r2,n:cpVect) :cpFloat; {$IFDEF USE_INLINE}inline;{$ENDIF}
procedure k_tensor(a,b: pcpBody; const r1,r2:cpVect; var k1,k2:cpVect);{$IFDEF USE_INLINE}inline;{$ENDIF}
procedure apply_impulses(a,b: PcpBody; r1,r2,j:cpVect);{$IFDEF USE_INLINE}inline;{$ENDIF}
procedure apply_bias_impulses(a,b: PcpBody; r1,r2,j: cpVect);{$IFDEF USE_INLINE}inline;{$ENDIF}
function J_MAX(constraint: pointer; const dt:cpFloat):cpFloat;

function cpBreakableJointAlloc : PcpBreakableJoint;
function cpBreakableJointNew(delegate:PcpConstraint; space:PcpSpace; AfreeDelegate: boolean):PcpConstraint;
function cpBreakableJointInit(breakable:PcpBreakableJoint; delegate:PcpConstraint; space:PcpSpace; AfreeDelegate: boolean):PcpConstraint;
procedure breakableJointPreStep(constraint: PcpConstraint; const dt,dt_inv:cpFloat);
procedure breakableJointApplyImpulse(constraint: PcpConstraint);
function breakableJointGetImpulse(constraint: PcpConstraint) : cpFloat;


function cpDampedSpringAlloc : PcpDampedSpring;
function cpDampedSpringReInit(spring: PcpDampedSpring; a,b: PcpBody; anchr1,anchr2 : cpVect;restLength,stiffness,damping : cpFloat) : PcpDampedSpring;
function cpDampedSpringInit(constraint: PcpDampedSpring; a,b: PcpBody; anchr1,anchr2 : cpVect;restLength,stiffness,damping : cpFloat) : PcpDampedSpring;
function cpDampedSpringNew(a,b: PcpBody; anchr1,anchr2: cpVect;restLength,stiffness,damping : cpFloat) : PcpConstraint;
procedure cpDampedSpringSetProperties(spring: PcpDampedSpring; restLength,stiffness,damping : cpFloat);
procedure cpDampedSpringGetProperties(spring: PcpDampedSpring; var restLength,stiffness,damping : cpFloat);
procedure DampedSpringPreStep(constraint:PcpConstraint; const dt,dt_inv:cpFloat);
procedure DampedSpringApplyImpulse(constraint:PcpConstraint);
function  DampedSpringGetImpulse(constraint:PcpConstraint) : cpFloat;

function cpPinJointAlloc : PcpPinJoint;
function cpPinJointReInit(joint: PcpPinJoint; a,b: PcpBody; anchr1,anchr2 : cpVect) : PcpPinJoint;
function cpPinJointInit(joint: PcpPinJoint; a,b: PcpBody; anchr1,anchr2 : cpVect) : PcpPinJoint; overload;
function cpPinJointInit(joint: PcpPinJoint; a,b: PcpBody; pivot : cpVect) : PcpPinJoint; overload;

function cpPinJointNew(a,b: PcpBody; anchr1,anchr2 : cpVect) : PcpConstraint; overload;
function cpPinJointNew(a,b: PcpBody; pivot : cpVect) : PcpConstraint; overload;

procedure pinJointPreStep(constraint: PcpConstraint; const dt,dt_inv:cpFloat);
procedure pinJointApplyImpulse(constraint: PcpConstraint);
function pinJointGetImpulse(constraint: PcpConstraint) : cpFloat;

function cpSlideJointAlloc : PcpSlideJoint;
function cpSlideJointReInit(joint: PcpSlideJoint; a,b : PcpBody; anchr1,anchr2 : cpVect; min,max : cpFloat) : PcpSlideJoint;
function cpSlideJointInit(joint: PcpSlideJoint; a,b : PcpBody; anchr1,anchr2 : cpVect; min,max : cpFloat) : PcpSlideJoint;
function cpSlideJointNew(a,b: PcpBody; anchr1,anchr2 : cpVect; min,max : cpFloat) : PcpConstraint;
procedure slideJointApplyImpulse(constraint: PcpConstraint);
procedure slideJointPreStep(constraint: PcpConstraint; const dt,dt_inv:cpFloat);
function slideJointGetImpulse(constraint : PcpConstraint) : cpFloat;

function cpPivotJointAlloc() : PcpPivotJoint;
function cpPivotJointReInit(joint : PcpPivotJoint; a,b : PcpBody; anchr1,anchr2 : cpVect) : PcpPivotJoint; overload;
function cpPivotJointReInit(joint : PcpPivotJoint; a,b : PcpBody; pivot : cpVect) : PcpPivotJoint; overload;
function cpPivotJointInit(joint : PcpPivotJoint; a,b : PcpBody; anchr1,anchr2 : cpVect) : PcpPivotJoint; overload;
function cpPivotJointInit(joint : PcpPivotJoint; a,b : PcpBody; pivot : cpVect) : PcpPivotJoint; overload;
function cpPivotJointNew2(a,b : PcpBody; anchr1,anchr2 : cpVect) : PcpConstraint;
function cpPivotJointNew(a,b : PcpBody; pivot : cpVect) : PcpConstraint;
procedure pivotJointApplyImpulse(constraint: PcpConstraint);
procedure pivotJointPreStep(constraint: PcpConstraint; const dt,dt_inv:cpFloat);
function pivotJointGetImpulse(constraint: PcpConstraint) : cpFloat;

function cpGrooveJointAlloc : PcpGrooveJoint;
function cpGrooveJointReInit(joint: PcpGrooveJoint; a,b : PcpBody; groove_a,groove_b,anchr2 : cpVect) : PcpGrooveJoint;
function cpGrooveJointInit(joint: PcpGrooveJoint; a,b : PcpBody; groove_a,groove_b,anchr2 : cpVect) : PcpGrooveJoint;
function cpGrooveJointNew(a,b: PcpBody; groove_a,groove_b,anchr2 : cpVect) : PcpConstraint;
procedure grooveJointApplyImpulse(constraint:PcpConstraint);
procedure grooveJointPreStep(constraint:PcpConstraint; const dt,dt_inv:cpFloat);
function grooveJointGetImpulse(constraint : PcpConstraint) : cpFloat;

function cpSimpleMotorAlloc : PcpSimpleMotor;
function cpSimpleMotorInit(joint: PcpSimpleMotor; a,b : PcpBody; rate : cpFloat) : PcpSimpleMotor;
function cpSimpleMotorNew(a,b: PcpBody; rate : cpFloat) : PcpConstraint;
procedure SimpleMotorPreStep(constraint: PcpConstraint; const dt,dt_inv:cpFloat);
procedure SimpleMotorApplyImpulse(constraint: PcpConstraint);
function SimpleMotorGetImpulse(constraint : PcpConstraint) : cpFloat;

function cpGearJointAlloc : PcpGearJoint;
function cpGearJointInit(joint: PcpGearJoint; a,b : PcpBody; phase,ratio : cpFloat) : PcpGearJoint;
function cpGearJointNew(a,b: PcpBody; phase,ratio : cpFloat) : PcpConstraint;
procedure GearJointPreStep(constraint: PcpConstraint; const dt,dt_inv:cpFloat);
procedure GearJointApplyImpulse(constraint: PcpConstraint);
function GearJointGetImpulse(constraint : PcpConstraint) : cpFloat;

function cpRotaryLimitJointAlloc : PcpRotaryLimitJoint;
function cpRotaryLimitJointInit(joint: PcpRotaryLimitJoint; a,b : PcpBody; min,max : cpFloat) : PcpRotaryLimitJoint;
function cpRotaryLimitJointNew(a,b: PcpBody; min,max : cpFloat) : PcpConstraint;
procedure RotaryLimitJointPreStep(constraint: PcpConstraint; const dt,dt_inv:cpFloat);
procedure RotaryLimitJointApplyImpulse(constraint: PcpConstraint);
function RotaryLimitJointGetImpulse(constraint : PcpConstraint) : cpFloat;

function cpDampedRotarySpringAlloc : PcpDampedRotarySpring;
function cpDampedRotarySpringInit(joint: PcpDampedRotarySpring; a,b : PcpBody; restAngle,stiffness,damping : cpFloat) : PcpDampedRotarySpring;
function cpDampedRotarySpringNew(a,b: PcpBody; restAngle,stiffness,damping : cpFloat) : PcpConstraint;
procedure DampedRotarySpringPreStep(constraint: PcpConstraint; const dt,dt_inv:cpFloat);
procedure DampedRotarySpringApplyImpulse(constraint: PcpConstraint);
function DampedRotarySpringGetImpulse(constraint : PcpConstraint) : cpFloat;



// *****************************************************************************************************************************
//
// Space functions
//
// *****************************************************************************************************************************

// Basic allocation/destruction functions.
function  cpSpaceAlloc : PcpSpace;
function  cpSpaceInit( space : PcpSpace) : PcpSpace;
function  cpSpaceNew : PcpSpace;

procedure cpSpaceClearArbiters ( space : PcpSpace);
procedure cpSpaceDestroy( space : PcpSpace);
procedure cpSpaceFree( space : PcpSpace);

// Convenience function. Frees all referenced entities. (bodies, shapes and joints)
procedure cpSpaceFreeChildren( space : PcpSpace);

// Collision pair function management functions.
procedure cpSpaceAddCollisionPairFunc( space : PcpSpace; a,b : LongWord; func : TcpCollFunc; data : pointer);
procedure cpSpaceRemoveCollisionPairFunc( space : PcpSpace; a,b : LongWord);
procedure cpSpaceSetDefaultCollisionPairFunc( space : PcpSpace; func : TcpCollFunc; data : pointer);
function cpContactSetReject(ptr : pointer; data : pointer) : integer;

// Add and remove entities from the system.
procedure cpSpaceAddShape( space : PcpSpace; shape : PcpShape);
procedure cpSpaceAddStaticShape( space : PcpSpace; shape : PcpShape);
procedure cpSpaceAddBody( space : PcpSpace; body : PcpBody);
procedure cpSpaceAddConstraint( space : PcpSpace; constraint : PcpConstraint);

procedure cpSpaceRemoveShape( space : PcpSpace; shape : PcpShape);
procedure cpSpaceRemoveStaticShape( space : PcpSpace; shape : PcpShape);
procedure cpSpaceRemoveBody( space : PcpSpace; body : PcpBody);
procedure cpSpaceRemoveConstraint( space : PcpSpace; constraint : PcpConstraint);
procedure cpSpaceEachBody( space : PcpSpace; func : TcpSpaceBodyIterator; data : pointer);

// Point query callback function
procedure cpSpaceShapePointQuery(space : PcpSpace; point:cpVect; func:cpSpacePointQueryFunc; data:pointer);
procedure cpSpaceStaticShapePointQuery(space : PcpSpace; const point:cpVect; func:cpSpacePointQueryFunc; data:pointer);


// Spatial hash management functions.
procedure cpSpaceResizeStaticHash( space : PcpSpace; const dim : cpFloat; const count : Integer);
procedure cpSpaceResizeActiveHash( space : PcpSpace; const dim : cpFloat; const count : Integer);
procedure cpUpdateBBCache(ptr:pointer; unused:pointer);
procedure cpSpaceRehashStatic( space : PcpSpace);

// Update the space.
procedure cpQueryFunc(p1,p2,data:pointer);
function cpQueryReject(a,b:pcpShape) : integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
procedure cpSpaceStep( space : PcpSpace; const dt : cpFloat);


function cpTotalBodies : integer;
function cpTotalShapes : integer;
function cpTotalArbiters : integer;
function cpTotalConstraints : integer;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
var Space: PcpSpace;

      cpPinJointClass:cpConstraintClass       = ( cpType:CP_PIN_JOINT;       preStep:PinJointPreStep; applyImpulse:pinJointApplyImpulse; getImpulse:pinJointGetImpulse);
      cpPivotJointClass:cpConstraintClass     = ( cpType:CP_PIVOT_JOINT;     preStep:PivotJointPreStep; applyImpulse:pivotJointApplyImpulse; getImpulse:pivotJointGetImpulse);
      cpSlideJointClass:cpConstraintClass     = ( cpType:CP_SLIDE_JOINT;     preStep:SlideJointPreStep; applyImpulse:slideJointApplyImpulse; getImpulse:slideJointGetImpulse);
      cpGrooveJointClass:cpConstraintClass    = ( cpType:CP_GROOVE_JOINT;    preStep:GrooveJointPreStep; applyImpulse:grooveJointApplyImpulse; getImpulse:grooveJointGetImpulse);

      cpDampedSpringClass:cpConstraintClass   = ( cpType:CP_SPRING_JOINT;    preStep:DampedSpringPreStep; applyImpulse:DampedSpringApplyImpulse; getImpulse:DampedSpringGetImpulse);
      cpBreakableJointClass:cpConstraintClass = ( cpType:CP_BREAKABLE_JOINT; preStep:BreakableJointPreStep; applyImpulse:BreakableJointApplyImpulse; getImpulse:BreakableJointGetImpulse);
      cpSimpleMotorClass:cpConstraintClass    = ( cpType:CP_SIMPLE_MOTOR_JOINT; preStep:SimpleMotorPreStep; applyImpulse:SimpleMotorApplyImpulse; getImpulse:SimpleMotorGetImpulse);
      cpGearJointClass:cpConstraintClass      = ( cpType:CP_GEAR_JOINT; preStep:GearJointPreStep; applyImpulse:GearJointApplyImpulse; getImpulse:GearJointGetImpulse);
      cpRotaryLimitJointClass:cpConstraintClass   = ( cpType:CP_ROTARY_LIMIT_JOINT; preStep:RotaryLimitJointPreStep; applyImpulse:RotaryLimitJointApplyImpulse; getImpulse:RotaryLimitJointGetImpulse);
      cpDampedRotarySpringClass:cpConstraintClass = ( cpType:CP_DAMPED_ROTARY_SPRING_JOINT; preStep:DampedRotarySpringPreStep; applyImpulse:DampedRotarySpringApplyImpulse; getImpulse:DampedRotarySpringGetImpulse);


      polyClass:cpShapeClass    =(cptype:CP_POLY_SHAPE;cacheData:cpPolyShapeCacheData;destroy:cpPolyShapeDestroy;pointQuery:cpPolyShapePointQuery);
      circleClass:cpShapeClass  =(cptype:CP_CIRCLE_SHAPE;cacheData:cpCircleShapeCacheData;destroy:nil;pointQuery:cpCircleShapePointQuery);
      segmentClass:cpShapeClass =(cptype:CP_SEGMENT_SHAPE;cacheData:cpSegmentShapeCacheData;destroy:nil;pointQuery:cpSegmentShapePointQuery);

implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
primes :array[0..29] of integer = (
  5,          //2^2  + 1
  11,         //2^3  + 3
  17,         //2^4  + 1
  37,         //2^5  + 5
  67,         //2^6  + 3
  131,        //2^7  + 3
  257,        //2^8  + 1
  521,        //2^9  + 9
  1031,       //2^10 + 7
  2053,       //2^11 + 5
  4099,       //2^12 + 3
  8209,       //2^13 + 17
  16411,      //2^14 + 27
  32771,      //2^15 + 3
  65537,      //2^16 + 1
  131101,     //2^17 + 29
  262147,     //2^18 + 3
  524309,     //2^19 + 21
  1048583,    //2^20 + 7
  2097169,    //2^21 + 17
  4194319,    //2^22 + 15
  8388617,    //2^23 + 9
  16777259,   //2^24 + 43
  33554467,   //2^25 + 35
  67108879,   //2^26 + 15
  134217757,  //2^27 + 29
  268435459,  //2^28 + 3
  536870923,  //2^29 + 11
  1073741827, //2^30 + 3
  0
);



var SHAPE_ID_COUNTER : integer=0;
    NumBodies : integer=0;
    NumShapes : integer=0;
    NumArbiters : integer=0;
    NumConstraints : integer=0;
    cpColfuncs : PcpCollisionFuncArray = nil;


// *****************************************************************************************************************************
//
// main functions
//
// *****************************************************************************************************************************

function cpTotalBodies : integer;
begin
   result:=NumBodies;
end;

function cpTotalShapes : integer;
begin
   result:=NumShapes;
end;

function cpTotalArbiters : integer;
begin
   result:=NumArbiters;
end;

function cpTotalConstraints : integer;
begin
   result:=NumConstraints;
end;

function calloc(Size : integer) : pointer;
begin
   getmem(result,size);
   fillchar(result^,size,0);
end;

function calloc(Num,ElemSize : integer) : pointer;
begin
   result:=calloc(num*ElemSize);
end;

procedure cfree(p : pointer);
begin
   if assigned(p) then freemem(p);
end;

function malloc(const Size : integer) : pointer;
begin
   getmem(result,size);
end;

function malloc(Num,ElemSize : integer) : pointer;
begin
   result:=malloc(num*ElemSize);
end;

function CP_HASH_PAIR(A, B :LongWord):LongWord;
begin
   result:=(A*CP_HASH_COEF) xor (B*CP_HASH_COEF);
end;

function CP_HASH_PAIR(A, B :pointer):LongWord;
begin
   result:=CP_HASH_PAIR(cardinal(A), cardinal(B));
end;

function CP_HASH_PAIR(A: Longword; B :pointer):LongWord;
begin
   result:=CP_HASH_PAIR(A, cardinal(B));
end;

function CP_HASH_PAIR(A: Pointer; B :Longword):LongWord;
begin
   result:=CP_HASH_PAIR(cardinal(A), B);
end;

function cpfmax (const a,b : cpFloat) : cpFloat;
begin
 if a>b then
  result:=a
 else
  result:=b;
end;

function cpfmin (const a,b : cpFloat) : cpFloat;
begin
 if a<b then
  result:=a
 else
  result:=b;
end;

function cpfclamp(const f,min,max : cpFloat) : cpFloat;
begin
   result:=cpfMin(cpfmax(f,min),max);
end;

function cpAddContactPoint(var arr:PcpContactArray; var max,num:integer) :pcpContact;
begin
   if not assigned(arr) then begin
      // Allocate the array if it hasn't been done.
      max := 2;
      num := 0;
      arr := calloc(max,sizeof(cpContact));
   end else if (num = max) then begin
      // Extend it if necessary.
      max := max*2;
      arr:=ReallocMemory(arr, max*sizeof(cpContact));
   end;

   result := @arr[num];
   inc(num);
end;

// Add contact points for circle to circle collisions.
// Used by several collision tests.
function circle2circleQuery(const p1,p2:cpVect; r1,r2:cpFloat; var con : PcpContactArray) : integer;
var distsq,non_zero_dist,dist, mindist : cpFloat;
    delta : cpVect;
begin
   result:=0;
   mindist := r1 + r2;
   delta := cpvsub(p2, p1);
   distsq := cpvlengthsq(delta);
   if(distsq >= mindist*mindist) then exit;

   dist := sqrt(distsq);
   // To avoid singularities, do nothing in the case of dist := 0.
   if dist<>0 then non_zero_dist := dist else non_zero_dist:=INFINITY;

   // Allocate and initialize the contact.
   con := calloc(sizeof(cpContact));
   cpContactInit(@con[0],
                 cpvadd(p1, cpvmult(delta, 0.5 + (r1 - 0.5*mindist)/non_zero_dist)),
                 cpvmult(delta, 1.0/non_zero_dist),
                 dist - mindist,
                 0
   );

   result:=1;
end;

// Collide circle shapes.
function circle2circle(shape1, shape2:pcpShape; var arr:PcpContactArray) : integer;
var circ1,circ2 : pcpCircleShape;
begin
   circ1 := pcpCircleShape(shape1);
   circ2 := pcpCircleShape(shape2);

   result:=circle2circleQuery(circ1.tc, circ2.tc, circ1.r, circ2.r, arr);
end;

// Collide circles to segment shapes.
function circle2segment(circleShape,segmentShape:pcpShape;var con:PcpContactArray) : integer;
var circ:pcpCircleShape;
    seg:pcpSegmentShape;
    rsum,dist,dt,dtMin,dtMax,dn : cpFloat;
    n : cpVect;
begin
   result:=0;
   circ := pcpCircleShape(circleShape);
   seg := pcpSegmentShape(segmentShape);

   //radius sum
   rsum:=circ.r+seg.r;

   // Calculate normal distance from segment.
   dn := cpvdot(seg.tn, circ.tc) - cpvdot(seg.ta, seg.tn);
   dist := abs(dn) - rsum;
   if(dist > 0.0) then exit;

   // Calculate tangential distance along segment.
   dt := -cpvcross(seg.tn, circ.tc);
   dtMin := -cpvcross(seg.tn, seg.ta);
   dtMax := -cpvcross(seg.tn, seg.tb);

   // Decision tree to decide which feature of the segment to collide with.
   if(dt < dtMin) then begin
      if(dt < (dtMin - rsum)) then begin
         exit;
      end else  begin
         result:=circle2circleQuery(circ.tc, seg.ta, circ.r, seg.r, con);
         exit;
      end;
   end else begin
      if(dt < dtMax) then begin
         if (dn < 0.0) then n := seg.tn else n:=cpvneg(seg.tn);
         con := calloc(sizeof(cpContact));
         cpContactInit(@con[0],
                       cpvadd(circ.tc, cpvmult(n, circ.r + dist*0.5)),
                       n,
                       dist,
                       0
         );
         result:=1;
         exit;
      end else begin
         if(dt < (dtMax + rsum))  then begin
            result:=circle2circleQuery(circ.tc, seg.tb, circ.r, seg.r, con);
            exit;
         end else begin
            exit;
         end;
      end;
   end;
   result:=1;
end;

// Like cpPolyValueOnAxis(), but for segments.
function segValueOnAxis(seg:pcpSegmentShape; n:cpVect; d:cpFloat):cpFloat;{$IFDEF USE_INLINE}inline;{$ENDIF}
var a, b:cpFloat;
begin
   a := cpvdot(n, seg.ta) - seg.r;
   b := cpvdot(n, seg.tb) - seg.r;
   result:=cpfmin(a, b) - d;
end;

// Identify vertexes that have penetrated the segment.
procedure findPointsBehindSeg(var arr:PcpContactArray; var max,num : integer; seg:pcpSegmentShape; poly:pcpPolyShape; pDist, coef:cpFloat);{$IFDEF USE_INLINE}inline;{$ENDIF}
var dta,dtb,dt : cpFloat;
    n,v : cpVect;
    i : integer;
begin
   dta := cpvcross(seg.tn, seg.ta);
   dtb := cpvcross(seg.tn, seg.tb);
   n := cpvmult(seg.tn, coef);

   for i:=0 to poly.numVerts-1 do begin
      v := poly.tVerts[i];
      if(cpvdot(v, n) < cpvdot(seg.tn, seg.ta)*coef + seg.r) then begin
         dt := cpvcross(seg.tn, v);
         if((dta >= dt) and (dt >= dtb)) then begin
            cpContactInit(cpaddContactPoint(arr, max, num), v, n, pDist, CP_HASH_PAIR(poly, i));
         end;
      end;
   end;
end;

// Identify points that have penetrated the segment.
procedure findPointBehindSeg(var arr:PcpContactArray; var max,num : integer; seg:pcpSegmentShape; v:cpVect; pDist, coef:cpFloat);{$IFDEF USE_INLINE}inline;{$ENDIF}
var dta,dtb,dt,vdn,nda : cpFloat;
    n : cpVect;
begin
   dta := cpvcross(seg.tn, seg.ta);
   dtb := cpvcross(seg.tn, seg.tb);
   n := cpvmult(seg.tn, coef);
   vdn:=cpvdot(v, n);
   nda:=cpvdot(seg.tn, seg.tb)*coef + seg.r;
   if(vdn < nda) then begin
      dt := cpvcross(seg.tn, v);
      if((dta >= dt) and (dt >= dtb)) then begin
         cpContactInit(cpaddContactPoint(arr, max, num), v, n, pDist, CP_HASH_PAIR(seg, @v));
      end;
   end;
end;

function seg2seg(shape1,shape2:pcpShape; var arr:PcpContactArray) : integer;
var seg1,seg2,ss,sd,st : pcpSegmentShape;
    max,num,i : integer;
    v : cpVect;
    ua,ub,pDist,denom,nume_a,nume_b : cpFloat;
begin
   result:=0;
   seg1 := pcpSegmentShape(shape1);
   seg2 := pcpSegmentShape(shape2);

   max := 0;
   num := 0;

   denom := ((seg2.tb.y - seg2.ta.y)*(seg1.tb.x - seg1.ta.x)) -
            ((seg2.tb.x - seg2.ta.x)*(seg1.tb.y - seg1.ta.y));

   nume_a := ((seg2.tb.x - seg2.ta.x)*(seg1.ta.y - seg2.ta.y)) -
             ((seg2.tb.y - seg2.ta.y)*(seg1.ta.x - seg2.ta.x));

   nume_b := ((seg1.tb.x - seg1.ta.x)*(seg1.ta.y - seg2.ta.y)) -
            ((seg1.tb.y - seg1.ta.y)*(seg1.ta.x - seg2.ta.x));


   if(abs(denom) < 0.001) then begin
      if((nume_a = 0.0) and (nume_b = 0.0)) then begin
//       COINCIDENT
         if nume_a>1 then;
         exit;
      end;
      if nume_a>1 then;
      exit;
   end;

   ua := nume_a / denom;
   ub := nume_b / denom;

   if((ua >= 0.0) and (ua <= 1.0) and (ub >= 0.0) and (ub <= 1.0)) then begin
      // Get the intersection point.
      v.x := seg1.ta.x + ua*(seg1.tb.x - seg1.ta.x);
      v.y := seg1.ta.y + ua*(seg1.tb.y - seg1.ta.y);
      if seg1.shape.is_static then begin
         ss:=seg1;
         sd:=seg2;
      end else begin
         if seg2.shape.is_static then begin
            ss:=seg2;
            sd:=seg1;
         end else begin
            ss:=seg2;
            sd:=seg1;
         end;
      end;
      for i:=0 to 1 do begin
         if cpvdot(ss.tn,cpvnormalize(cpvsub(sd.ta,ss.ta))) <=0 then begin
            pDist:=-cpvlength(cpvsub(sd.ta,v))+ss.r;
            cpContactInit(cpAddContactPoint(arr, max, num), sd.ta, cpvneg(ss.tn), pDist, CP_HASH_PAIR(cardinal(sd), 0));
         end;
         if cpvdot(ss.tn,cpvnormalize(cpvsub(sd.tb,ss.ta))) <=0 then begin
            pDist:=-cpvlength(cpvsub(sd.tb,v))+ss.r;
            cpContactInit(cpAddContactPoint(arr, max, num), sd.tb, cpvneg(ss.tn), pDist, CP_HASH_PAIR(cardinal(sd), 1));
         end;
         if num>0 then break;
         st:=ss;
         ss:=sd;
         sd:=st;
      end;

      result:=num;
   end;
end;

// Find the minimum separating axis for the give poly and axis list.
function findMSA(poly:pcpPolyShape; axes:pcpPolyShapeAxisArray; num:integer; var min_out:cpFloat):integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
var i,min_index : integer;
    dist,min : cpFloat;
begin
   result:=-1;
   min_index := 0;
   min := cpPolyShapeValueOnAxis(poly, axes[0].n, axes[0].d);
   if(min > 0.0) then exit;

   for i:=1 to num-1 do begin
      dist := cpPolyShapeValueOnAxis(poly, axes[i].n, axes[i].d);
      if(dist > 0.0)  then begin
         exit;
      end else if(dist > min) then begin
         min := dist;
         min_index := i;
      end;
   end;

   min_out := min;
   result:=min_index;
end;

// Add contacts for penetrating vertexes.
function findVerts(var arr:PcpContactArray; poly1,poly2:pcpPolyShape; n:cpVect; dist:cpFloat) : integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
var i,max,num : integer;
    v : cpVect;
begin
   max := 0;
   num := 0;

   for i:=0 to  poly1.numVerts-1 do begin
      v := poly1.tVerts[i];
      if (cpPolyShapeContainsVertPartial(poly2, v, cpvneg(n))<>0) then
         cpContactInit(cpaddContactPoint(arr, max, num), v, n, dist, CP_HASH_PAIR(poly1, i));
   end;

   for i:=0 to poly2.numVerts-1 do begin
      v := poly2.tVerts[i];
      if (cpPolyShapeContainsVertPartial(poly1, v,n)<>0) then
         cpContactInit(cpaddContactPoint(arr, max, num), v, n, dist, CP_HASH_PAIR(poly2, i));
   end;

   result:=num;
end;

// Collide poly shapes together.
function poly2poly(shape1,shape2:pcpShape; var arr:PcpContactArray) : integer;
var poly1,poly2:pcpPolyShape;
    min1,min2 : cpFloat;
    mini1,mini2 : integer;
begin
   result:=0;
   poly1 := pcpPolyShape(shape1);
   poly2 := pcpPolyShape(shape2);

   mini1 := findMSA(poly2, poly1.tAxes, poly1.numVerts, min1);
   if(mini1 = -1) then exit;

   mini2 := findMSA(poly1, poly2.tAxes, poly2.numVerts, min2);
   if(mini2 = -1) then exit;

   // There is overlap, find the penetrating verts
   if(min1 > min2) then result:=findVerts(arr, poly1, poly2, poly1.tAxes[mini1].n, min1)
      else result:=findVerts(arr, poly1, poly2, cpvneg(poly2.tAxes[mini2].n), min2);
end;

// This one is complicated and gross. Just don't go there...
// TODO: Comment me!
function seg2poly(shape1,shape2:pcpShape; var arr:PcpContactArray) : integer;
var seg :pcpSegmentShape;
    poly:pcpPolyShape;
    axes :pcpPolyShapeAxisArray;
    dist,poly_min,minNorm,minNeg,segD : cpFloat;
    max,num,i,mini : integer;
    va,vb,poly_n : cpVect;
    poly_a,poly_b : cpVect;
begin
   result:=0;
   seg := pcpSegmentShape(shape1);
   poly := pcpPolyShape(shape2);
   axes := poly.tAxes;

   segD := cpvdot(seg.tn, seg.ta);
   minNorm := cpPolyShapeValueOnAxis(poly, seg.tn, segD) - seg.r;
   minNeg := cpPolyShapeValueOnAxis(poly, cpvneg(seg.tn), -segD) - seg.r;
   if((minNeg > 0.0) or (minNorm > 0.0)) then exit;

   mini := 0;
   poly_min := segValueOnAxis(seg, axes[0].n, axes[0].d);
   if(poly_min > 0.0) then exit;
   for i:=0 to poly.numVerts-1 do begin
      dist := segValueOnAxis(seg, axes[i].n, axes[i].d);
      if(dist > 0.0) then begin
         exit;
      end else if(dist > poly_min) then begin
         poly_min := dist;
         mini := i;
      end;
   end;

   max := 0;
   num := 0;

   poly_n := cpvneg(axes[mini].n);

   va := cpvadd(seg.ta, cpvmult(poly_n, seg.r));
   vb := cpvadd(seg.tb, cpvmult(poly_n, seg.r));
   if(cpPolyShapeContainsVert(poly, va)<>0) then
      cpContactInit(cpaddContactPoint(arr, max, num), va, poly_n, poly_min, CP_HASH_PAIR(seg, 0));
   if(cpPolyShapeContainsVert(poly, vb)<>0) then
      cpContactInit(cpaddContactPoint(arr, max, num), vb, poly_n, poly_min, CP_HASH_PAIR(seg, 1));

   // Floating point precision problems here.
   // This will have to do for now.
   poly_min := poly_min - cp_collision_slop;
   if((minNorm >= poly_min) or (minNeg >= poly_min))  then begin
      if(minNorm > minNeg) then
         findPointsBehindSeg(arr, max, num, seg, poly, minNorm, 1.0)
      else
         findPointsBehindSeg(arr, max, num, seg, poly, minNeg, -1.0);
   end;


  // If no other collision points are found, try colliding endpoints.
  if(num = 0) then begin
      result:=1;
    poly_a := poly.tVerts[mini];
    poly_b := poly.tVerts[(mini + 1) mod poly.numVerts];

    if(circle2circleQuery(seg.ta, poly_a, seg.r, 0.0, arr)<>0) then exit;
    if(circle2circleQuery(seg.tb, poly_a, seg.r, 0.0, arr)<>0) then exit;
    if(circle2circleQuery(seg.ta, poly_b, seg.r, 0.0, arr)<>0) then exit;
    if(circle2circleQuery(seg.tb, poly_b, seg.r, 0.0, arr)<>0) then exit;
  end;

   result:=num;
end;

// This one is less gross, but still gross.
// TODO: Comment me!

function circle2poly(shape1, shape2:pcpShape; var con:PcpContactArray) : integer;
var circ:pcpCircleShape;
    poly:pcpPolyShape;
    axes:pcpPolyShapeAxisArray;
    i,mini : integer;
    dta,dtb,dt,dist,min : cpFloat;
    a,b,n:cpVect;
begin
   result:=0;
   circ := pcpCircleShape(shape1);
   poly := pcpPolyShape(shape2);
   axes := poly.tAxes;

   mini := 0;
   min := cpvdot(axes[0].n, circ.tc) - axes[0].d - circ.r;
   for i:=0 to poly.numVerts-1 do begin
      dist := cpvdot(axes[i].n, circ.tc) - axes[i].d - circ.r;
      if(dist > 0.0) then begin
         exit;
      end else if(dist > min) then begin
         min := dist;
         mini := i;
      end;
   end;

   n := axes[mini].n;
   a := poly.tVerts[mini];
   b := poly.tVerts[(mini + 1) mod poly.numVerts];
   dta := cpvcross(n, a);
   dtb := cpvcross(n, b);
   dt := cpvcross(n, circ.tc);

   if(dt < dtb) then begin
      result:=circle2circleQuery(circ.tc, b, circ.r, 0.0, con);
   end else if(dt < dta)  then begin
      con := calloc(sizeof(cpContact));
      cpContactInit(@con[0],
                     cpvsub(circ.tc, cpvmult(n, circ.r + min/2.0)),
                     cpvneg(n),
                     min,
                     0
      );
      result:=1;
      exit;
   end else begin
      result:=circle2circleQuery(circ.tc, a, circ.r, 0.0, con);
      exit;
   end;
end;

function cpCollideShapes( a : PcpShape; b : PcpShape; var arr : PcpContactArray) : integer;
var cfunc : TcpCollisionFunc;
begin
   result:=0;

   // Their shape types must be in order.
   assert(a.klass.cptype <= b.klass.cptype);

   cfunc := cpcolfuncs[a.klass.cptype + b.klass.cptype];
   if not assigned(cfunc) then exit;

   result:=cfunc(a, b, arr);
end;

procedure cpAddColFunc(a, b:cpShapeType; func:TcpCollisionFunc);
begin
   cpColfuncs[a + b] := func;
end;

procedure cpInitCollisionFuncs;
begin
    if assigned(cpColfuncs) then exit;
    cpColfuncs := calloc(50, sizeof(TcpCollisionFunc));

    cpaddColFunc(CP_CIRCLE_SHAPE,  CP_CIRCLE_SHAPE,  circle2circle);  //0=0
    cpaddColFunc(CP_CIRCLE_SHAPE,  CP_SEGMENT_SHAPE, circle2segment); //1=3
    cpAddColFunc(CP_SEGMENT_SHAPE, CP_SEGMENT_SHAPE, seg2seg);        //2=6
    cpaddColFunc(CP_CIRCLE_SHAPE,  CP_POLY_SHAPE,    circle2poly);    //3=9
    cpaddColFunc(CP_SEGMENT_SHAPE, CP_POLY_SHAPE,    seg2poly);       //4=12
    cpaddColFunc(CP_POLY_SHAPE,    CP_POLY_SHAPE,    poly2poly);      //6=18
end;

procedure cpInitChipmunk;
begin
   cpInitCollisionFuncs;
end;

procedure cpShutdownChipmunk;
begin
   cfree(cpColfuncs);
end;

function cpMomentForCircle(const m,r1,r2 : cpFloat; const offset : cpVect) : cpFloat;
begin
   result:=0.5*m*(r1*r1+r2*r2)+m*cpvdot(offset,offset);
end;

function cpCircleArea(const r : cpFloat) : cpFloat;
begin
   result:=M_PI*r*r;
end;

function cpCircleMass(const r,Density : cpFloat) : cpFloat;
begin
   result:=cpCircleArea(r)*Density
end;

function cpMomentForSegment(const m:cpFloat; const a,b:cpVect) : cpFloat;
var length:cpFloat;
    offset:cpVect;
begin
  length := cpvlength(cpvsub(b, a));
  offset := cpvmult(cpvadd(a, b), 1.0/2.0);
  result:=m*length*length/12.0 + m*cpvdot(offset, offset);
end;

function cpPolyArea(verts : pcpVectArray; const numVerts : integer) : cpFloat;
var i : integer;
begin
   result:=1;
   if numVerts<3 then exit;
   result:= ( verts[0].x * verts[numVerts-1].y - verts[numVerts-1].x * verts[0].y) * 0.5;
   for i:=1 to numVerts-1 do begin
      result:= result +abs( verts[i].x * verts[i-1].y - verts[i-1].x * verts[i].y) * 0.5;
   end;
end;

function cpPolyMass(verts : pcpVectArray; const numVerts : integer; const Density : cpFloat) : cpFloat;
begin
   result:=cpPolyArea(Verts,numVerts)*Density;
end;

function cpMomentForPoly(const m : cpFloat; const numVerts : integer; verts : PcpVectArray; const offset : cpVect) : cpFloat;
var i : integer;
    sum1,sum2,a,b : cpFloat;
    tverts:PcpVectArray;
    v1,v2 : cpVect;
begin
   tVerts:=malloc(numVerts,sizeof(cpVect));
   for i:=0 to numVerts-1 do tVerts[i] := cpvadd(verts[i], offset);

   sum1 := 0.0;
   sum2 := 0.0;
   for i:=0 to numVerts-1 do begin
      v1 := tVerts[i];
      v2 := tVerts[(i+1) mod numVerts];

      a := cpvcross(v2, v1);
      b := cpvdot(v1, v1) + cpvdot(v1, v2) + cpvdot(v2, v2);

      sum1:=sum1+a*b;
      sum2:=sum2+a;
   end;

   freemem(tVerts);
   result:=(m*sum1)/(6.0*sum2);
end;

// *****************************************************************************************************************************
//
// Vect functions
//
// *****************************************************************************************************************************

function cpv(const x,y : cpFloat):cpVect;
begin
   result.x:=x; result.y:=y;
end;

function cpvadd(const v1,v2 : cpVect):cpVect;
begin
   result:=cpv(v1.x+v2.x,v1.y+v2.y);
end;

function cpvneg(const v : cpVect):cpVect;
begin
   result:=cpv(-v.x,-v.y);
end;

function cpvsub(const v1,v2 : cpVect):cpVect;
begin
   result:=cpv(v1.x-v2.x,v1.y-v2.y);
end;

function cpvmult(const v:cpVect; const s : cpFloat):cpVect;
begin
   result:=cpv(v.x*s,v.y*s);
end;

function cpvdot(const v1,v2 : cpVect):cpFloat;
begin
   result:=v1.x*v2.x + v1.y*v2.y;
end;

function cpvcross(const v1,v2 : cpVect):cpFloat;
begin
   result:=v1.x*v2.y - v1.y*v2.x;
end;

function cpvperp(const v : cpVect):cpVect;
begin
   result:=cpv(-v.y,v.x);
end;

function cpvrperp(const v : cpVect):cpVect;
begin
   result:=cpv(v.y,-v.x);
end;

function cpvproject(const v1,v2 : cpVect):cpVect;
begin
   result:=cpvmult(v2, cpvdot(v1, v2)/cpvdot(v2, v2));
end;

function cpvlerp(const v1,v2 : cpVect; const dt : cpFloat):cpVect;
begin
   result:=cpvadd(v1,cpvmult(cpvsub(v2,v1),dt));
end;

function cpvrotate(const v1,v2 : cpVect):cpVect;
begin
   result:=cpv(v1.x*v2.x - v1.y*v2.y, v1.x*v2.y + v1.y*v2.x);
end;

function cpvunrotate(const v1,v2 : cpVect):cpVect;
begin
   result:=cpv(v1.x*v2.x + v1.y*v2.y, v1.y*v2.x - v1.x*v2.y);
end;

function cpvforangle(const a : cpFloat) : cpVect;
begin
   result.x:=cos(a);
   result.y:=sin(a);
end;

function cpvtoangle(const v : cpVect) : cpFloat;
begin
   result:=arctan2(v.y,v.x);
end;

function cpvstr(const v : cpVect) : string;
  var
    x, y : String;
begin
  Str( v.x:0:3, x );
  Str( v.x:0:3, y );
  Result := x + ', ' + y;
end;

function cpvlength(const v : cpVect) : cpFloat;
begin
   result:=sqrt(cpvdot(v,v));
end;

function cpvlengthsq(const v : cpVect) : cpFloat;
begin
   result:=cpvdot(v,v);
end;

function cpvnormalize(const v : cpVect) : cpVect;
begin
   result:=cpvmult(v,1/cpvlength(v));
end;

function cpvdist(const v1,v2 : cpVect) : cpFloat;
begin
   result:=cpvLength(cpvsub(v1,v2));
end;

function cpvdistsq(const v1,v2 : cpVect) : cpFloat;
begin
   result:=cpvLengthsq(cpvsub(v1,v2));
end;

function cpvEqual(const v1,v2 : cpVect) : boolean;
begin
   result:=(v1.x=v2.x) and (v1.y=v2.y);
end;

function modf(const a,b: cpFloat) : cpFloat;
begin
   result:=a-(b*int(a/b));
end;

// *****************************************************************************************************************************
//
// BB functions
//
// *****************************************************************************************************************************

function cpBBNew (const l,b,r,t : cpFloat):cpBB;
begin
   result.l:=l;
   result.b:=b;
   result.r:=r;
   result.t:=t;
end;

function cpBBintersects(const a,b : cpBB):integer;
begin
 if ((a.l<=b.r) AND (b.l<=a.r) AND (a.b<=b.t) AND (b.b<=a.t)) then
  result:=1
 else
  result:=0;
end;

function cpBBcontainsBB(const bb,other : cpBB):integer;
begin
 if ((bb.l < other.l) AND (bb.r > other.r) AND (bb.b < other.b) AND (bb.t > other.t)) then
  result:=1
 else
  result:=0;
end;

function cpBBcontainsVect(const bb : cpBB; const v : cpVect):integer;
begin
   if ((bb.l < v.x) AND (bb.r > v.x) AND (bb.b < v.y) AND (bb.t > v.y)) then
     result:=1
   else
     result:=0;
end;

function  cpBBClampVect(const bb : cpBB; const v : cpVect) : cpVect;
begin
   result:=cpv(cpfmin(cpfmax(bb.l,v.x),bb.r),cpfmin(cpfmax(bb.b,v.y),bb.t));
end;

function  cpBBWrapVect(const bb : cpBB; const v : cpVect) : cpVect;
var ix,modx,x,iy,mody,y : cpFloat;
begin
   ix := abs(bb.r - bb.l);
   modx := modf(v.x - bb.l, ix);
   if (modx > 0.0) then x := modx else x:=modx + ix;

   iy := abs(bb.t - bb.b);
   mody := modf(v.y - bb.b, iy);
   if (mody > 0.0) then y := mody else y:= mody + iy;

   result:=cpv(x + bb.l, y + bb.b);
end;

// ****************************************************************************************************************************
//
// Body functions
//
// *****************************************************************************************************************************

function cpBodyAlloc() : PcpBody;
begin
   result:=calloc(sizeof(result^));
   result.shapes:=cpArrayNew(4);
end;

function cpBodyInit( body : PcpBody; const m,i : cpFloat) : PcpBody;
begin
   inc(NumBodies);

   body.velocity_func:=cpBodyUpdateVelocity;
   body.position_func:=cpBodyUpdatePosition;

   cpBodySetMass(body, m);
   cpBodySetMoment(body, i);

   body.p := cpvzero;
   body.v := cpvzero;
   body.f := cpvzero;

   cpBodySetAngle(body, 0.0);

   body.w := 0.0;
   body.t := 0.0;

   body.v_bias := cpvzero;
   body.w_bias := 0.0;

   body.data:=nil;

// body.active = 1;

   result:=body;
end;

function cpBodyNew( const m,i : cpFloat) : PcpBody;
begin
   result:=cpBodyInit(cpBodyAlloc,m,i);
end;

procedure cpBodyDestroy( body : PcpBody);
//var i: integer;
begin
   //for i:=0 to body.shapes.num-1 do pcpShape(body.shapes.arr[i]).body:=nil;
   cpArrayFree(body.shapes);
end;

procedure cpBodyFree( body : PcpBody);
begin
   if assigned(body) then cpBodyDestroy(body);
   cfree(body);
   dec(NumBodies);
end;

procedure  cpBodySetMass( body : PcpBody; const m : cpFloat);
begin
   body.m:=m;
   body.m_inv:=1/m;
end;

procedure  cpBodySetMoment( body : PcpBody; const i : cpFloat);
begin
   body.i:=i;
   body.i_inv:=1/i;
end;

procedure cpBodySetAngle( body : PcpBody; const a : cpFloat);
begin
   body.a:=modf(a,M_2PI);
   body.rot:=cpvforangle(a);
end;

procedure cpBodySlew( body : PcpBody; const pos : cpVect; const dt : cpFloat);
var delta :cpVect;
begin
   delta := cpvsub(pos,body.p);
   body.v := cpvmult(delta, 1.0/dt);
end;

procedure cpBodyUpdateVelocity( body : PcpBody; const gravity : cpVect; const damping,dt : cpFloat);
begin
   body.v := cpvadd(cpvmult(body.v, damping), cpvmult(cpvadd(gravity, cpvmult(body.f, body.m_inv)), dt));
   body.w := body.w*damping + body.t*body.i_inv*dt;
end;

procedure cpBodyUpdatePosition( body : PcpBody; const dt : cpFloat);
begin
   body.p := cpvadd(body.p, cpvmult(cpvadd(body.v, body.v_bias), dt));
   cpBodySetAngle(body, body.a + (body.w + body.w_bias)*dt);

   body.v_bias := cpvzero;
   body.w_bias := 0.0;
end;

procedure cpBodyResetForces( body : PcpBody);
begin
   body.f:=cpvzero;
   body.t:=0;
end;

procedure cpBodyApplyForce( body : PcpBody; const f,r : cpVect);
begin
   body.f := cpvadd(body.f, f);
   body.t := body.t+cpvcross(r, f);
end;

procedure cpApplyDampedSpring( a,b : PcpBody; const anchr1,anchr2 : cpVect; const rlen, k, dmp, dt : cpFloat);
var r1,r2,delta,v1,v2,n,f : cpVect;
    dist,f_spring,vrn,f_damp : cpFloat;
begin
   // Calculate the world space anchor coordinates.
   r1 := cpvrotate(anchr1, a.rot);
   r2 := cpvrotate(anchr2, b.rot);

   delta := cpvsub(cpvadd(b.p, r2), cpvadd(a.p, r1));
   dist := cpvlength(delta);
   if dist>0 then n :=cpvmult(delta, 1.0/dist) else n:=cpvzero;

   f_spring := (dist - rlen)*k;

   // Calculate the world relative velocities of the anchor points.
   v1 := cpvadd(a.v, cpvmult(cpvperp(r1), a.w));
   v2 := cpvadd(b.v, cpvmult(cpvperp(r2), b.w));

   // Calculate the damping force.
   // This really should be in the impulse solver and can produce problems when using large damping values.
   vrn := cpvdot(cpvsub(v2, v1), n);
   f_damp := vrn*cpfmin(dmp, 1.0/(dt*(a.m_inv + b.m_inv)));

   // Apply!
   f := cpvmult(n, f_spring + f_damp);
   cpBodyApplyForce(a, f, r1);
   cpBodyApplyForce(b, cpvneg(f), r2);
end;

// Convert body local to world coordinates
function cpBodyLocal2World( body : PcpBody; const v : cpVect):cpVect;
begin
   result:=cpvadd(body.p, cpvrotate(v, body.rot));
end;

// Convert world to body local coordinates
function cpBodyWorld2Local( body : PcpBody; const v : cpVect):cpVect;
begin
   result:=cpvunrotate(cpvsub(v, body.p), body.rot);
end;

// Apply an impulse (in world coordinates) to the body.
procedure cpBodyApplyImpulse( body : PcpBody; const j,r : cpVect);
begin
   body.v:= cpvadd(body.v, cpvmult(j, body.m_inv));
   body.w:= body.w+(body.i_inv*cpvcross(r, j));
end;

// Not intended for external use. Used by cpArbiter.c and cpJoint.c.
procedure cpBodyApplyBiasImpulse( body : PcpBody; const j,r : cpVect);
begin
   body.v_bias:= cpvadd(body.v_bias, cpvmult(j, body.m_inv));
   body.w_bias:= body.w_bias+(body.i_inv*cpvcross(r, j));
end;

procedure ResetBodyForces(ptr : pointer; data : pointer);
begin
   cpBodyResetForces(ptr);
end;

procedure cpResetForcesAllBodies( Space :pcpSpace );
begin
    cpArrayEach(space.bodies,ResetBodyForces,nil);
end;

function cpvBodyDist(b1,b2 : pcpBody) : cpFloat;
begin
   result:=cpvdist(b1.p,b2.p);
end;

// *****************************************************************************************************************************
//
// Array functions
//
// *****************************************************************************************************************************
// NOTE: cpArray is rarely used and will probably go away.

function  cpArrayAlloc : PcpArray;
begin
   result:=calloc(sizeof(result^));
end;

function cpArrayInit( arr : PcpArray; size : integer ) : PcpArray;
begin
   arr.num := 0;

   if size=0 then size := 4;
   arr.max := size;
   arr.arr:=calloc(size,sizeof(pointer));

   result:=arr;
end;

function  cpArrayNew( const size :integer) : PcpArray;
begin
   result:=cpArrayInit(cpArrayAlloc,size);
end;

procedure cpArrayDestroy( arr : PcpArray );
begin
   cfree(arr.arr);
end;

procedure cpArrayFree( arr : PcpArray );
begin
   if not assigned(arr) then exit;
   cpArrayDestroy(arr);
   cfree(arr);
end;

procedure cpArrayPush( arr : PcpArray; cpobject : pointer );
begin
   if(arr.num = arr.max) then begin
      arr.max:=arr.max*2;
      arr.arr := ReallocMemory(arr.arr, arr.max*sizeof(pointer));
//      fillchar(arr.arr[arr.num],(arr.max-arr.num)*sizeof(pointer),0);
   end;

   arr.arr[arr.num] := cpobject;
   inc(arr.num);
end;

procedure cpArrayDeleteIndex( arr : PcpArray; const index :integer);
var last : integer;
begin
   dec(arr.num);
   last := arr.num;
   arr.arr[index] := arr.arr[last];
end;

procedure cpArrayDeleteObj( arr : PcpArray; obj : pointer );
var i : integer;
begin
   for i:=0 to arr.num-1 do begin
       if(arr.arr[i] = obj) then begin
        cpArrayDeleteIndex(arr, i);
         exit;
      end;
   end;
end;

procedure cpArrayEach( arr : PcpArray; iterFunc : TcpArrayIter; data : pointer );
var i : integer;
begin
   i:=0;
   while i<arr.num do begin
      iterFunc(arr.arr[i], data);
      inc(i);
   end;
end;

function cpArrayContains( arr : PcpArray; ptr : pointer ) : integer;
var i : integer;
begin
   result:=0;
   for i:=0 to arr.num-1 do begin
      if(arr.arr[i] = ptr) then begin
         result:=1;
         exit;
      end;
   end;
end;

// *****************************************************************************************************************************
//
//  HashSet functions
//
// *****************************************************************************************************************************

function next_prime(n:integer) : integer;
var i : integer;
begin
   i:=0;
   while(n > primes[i]) do begin
      inc(i);
      assert(primes[i]<>0); // realistically this should never happen
   end;

   result:=primes[i];
end;

function cpHashSetAlloc : PcpHashSet;
begin
   result:=calloc(sizeof(result^));
end;

function cpHashSetInit( cpset : PcpHashSet; size : integer; eqlFunc : TcpHashSetEqlFunc; trans : TcpHashSetTransFunc) : PcpHashSet;
begin
   cpset.size := next_prime(size);
   cpset.entries := 0;

   cpset.eql := eqlFunc;
   cpset.trans := trans;

   cpset.default_value := nil;

   cpset.table:=calloc(cpset.size,sizeof(cpHashSetBin));

   result:=cpset;
end;

function cpHashSetNew( const size : integer; eqlFunc : TcpHashSetEqlFunc; trans : TcpHashSetTransFunc) : PcpHashSet;
begin
   result:=cpHashSetInit(cpHashSetAlloc, size, eqlFunc, trans);
end;

procedure cpHashSetDestroy( cpset : PcpHashSet);
var i : integer;
    bin,next:pcpHashSetBin;
begin
   // Free the chains.
   for i:=0 to cpset.size-1 do begin
      // Free the bins in the chain.
      bin := cpset.table[i];
      while assigned(bin) do begin
         next := bin.next;
         cfree(bin);
         bin := next;
      end;
   end;

   // Free the table.
   cfree(cpset.table);
end;

procedure cpHashSetFree( cpset : PcpHashSet);
begin
   if assigned(cpset) then cpHashSetDestroy( cpset );
   cfree(cpset);
end;

function cpHashSetIsFull( cpset : PcpHashSet) : boolean;
begin
   result:=cpset.entries >= cpset.size
end;

procedure cpHashSetResize( cpset : PcpHashSet );
var i,index,newSize : longword;
    next,bin : pcpHashSetBin;
    newTable : PcpHashSetBinArray;
begin
   // Get the next approximate doubled prime.
   newSize := next_prime(cpset.size + 1);
   // Allocate a new table.
   newTable:=calloc(newSize,sizeof(cpHashSetBin));

   // Iterate over the chains.
   for i:=0 to cpset.size-1 do begin
     // Rehash the bins into the new table.
     bin := cpset.table[i];
     while assigned(bin) do begin
         next := bin.next;
         index := bin.hash mod newSize;
         bin.next := newTable[index];
         newTable[index] := bin;
         bin := next;
      end;
   end;

   cfree(cpset.table);

   cpset.table := newTable;
   cpset.size := newSize;
end;

function  cpHashSetInsert( cpset : PcpHashSet; hash : LongWord; ptr : pointer; data : pointer) : pointer;
var index : longword;
    bin:pcpHashSetBin;
begin
   {$WARNINGS OFF}
   index := hash mod cpset.size;
   {$WARNINGS ON}

   // Find the bin with the matching element.
   bin := cpset.table[index];
   while assigned(bin) and not cpset.eql(ptr, bin.elt) do  bin := bin.next;

   // Create it necessary.
   if not assigned(bin) then begin
      bin:=calloc(sizeof(cpHashSetBin));
      bin.hash := hash;
      bin.elt := cpset.trans(ptr, data); // Transform the pointer.

      bin.next := cpset.table[index];
      cpset.table[index] := bin;

      inc(cpset.entries);

      // Resize the set if it's full.
      if cpHashsetIsFull(cpset) then cpHashSetResize(cpset);
   end;

   result:=bin.elt;
end;

function  cpHashSetRemove( cpset : PcpHashSet; hash : LongWord; ptr : pointer) : pointer;
var index : integer;
    bin : pcpHashSetBin;
    prev_ptr: ppcpHashSetBin;
begin
   {$WARNINGS OFF}
   index := hash mod cpset.size;
   {$WARNINGS ON}

   // Pointer to the previous bin pointer.
   prev_ptr := @cpset.table[index];
   // Pointer the the current bin.
   bin := cpset.table[index];

   // Find the bin
   while assigned(bin) and (not cpset.eql(ptr, bin.elt)) do begin
      prev_ptr := @bin.next;
      bin := bin.next;
   end;

   // Remove it if it exists.
   if assigned(bin) then begin
      // Update the previous bin pointer to point to the next bin.
      prev_ptr^ := bin.next;
      dec(cpset.entries);

      result:=bin.elt;
      // FIXME: *bin = (cpHashSetBin){};
      // FillChar( bin^, SizeOf( cpHashSetBin ), 0 );
      cfree(bin);
      exit;
   end;

   result:=nil;
end;

function cpHashSetFind( cpset : PcpHashSet; hash : LongWord; ptr : pointer) : pointer;
var index : integer;
    Bin:pcpHashSetBin;
begin
   {$WARNINGS OFF}
   index := hash mod cpset.size;
   {$WARNINGS ON}
   bin := cpset.table[index];
   while assigned(bin) and (not cpset.eql(ptr, bin.elt)) do bin := bin.next;
   if assigned(bin) then result:=bin.elt else result:=cpset.default_value;
end;

procedure cpHashSetEach( cpset : PcpHashSet; func : TcpHashSetIterFunc; data : pointer);
  var
    i    : integer;
    bin  : pcpHashSetBin;
    next : pcpHashSetBin;
begin
  for i := 0 to cpset.size-1 do
    begin
      bin := cpset.table[ i ];
      while assigned(bin) do
        begin
          next := bin.next;
          func( bin.elt, data );
          bin := next;
        end;
    end;
end;

procedure cpHashSetReject( cpset : PcpHashSet; func : TcpHashSetRejectFunc; data : pointer);
var i : integer;
    next,bin:pcpHashSetBin;
    prev_ptr:ppcpHashSetBin;
begin
   // Iterate over all the chains.
   for i:=0 to cpset.size-1 do begin
      // The rest works similarly to cpHashcpsetRemove() above.
      prev_ptr := @cpset.table[i];
      bin := cpset.table[i];
      while assigned(bin) do begin
         next := bin.next;
         if(func(bin.elt, data)<>0) then begin
            prev_ptr := @bin.next;
         end else begin
            prev_ptr^ := next;
            dec(cpset.entries);
            cfree(bin);
         end;
         bin := next;
      end;
   end;
end;

// *****************************************************************************************************************************
//
//  SpaceHash functions
//
// *****************************************************************************************************************************

procedure cpSpaceHashAllocTable(hash:pcpSpaceHash; numcells: integer);
begin
   cfree(hash.table);
   hash.numcells := numcells;
   hash.table:=calloc(numcells,sizeof(pcpSpaceHashBin));
end;

function cpSpaceHashAlloc : PcpSpaceHash;
begin
   result:=calloc(sizeof(result^));
end;

function cpHandleAlloc:pcpHandle;
begin
   result:=calloc(sizeof(result^));
end;

function cpHandleInit(hand:pcpHandle; obj:pointer):pcpHandle;
begin
   hand.obj := obj;
   hand.retain := 0;
   hand.stamp := 0;

   result:=hand;
end;

function cpHandleNew(obj:pointer):pcpHandle;
begin
   result:=cpHandleInit(cpHandleAlloc, obj);
end;

procedure cpHandleRetain(hand:pcpHandle);
begin
   inc(hand.retain);
end;

procedure cpHandleFree(hand:pcpHandle);
begin
   cfree(hand);
end;

procedure cpHandleRelease(hand:pcpHandle);
begin
   dec(hand.retain);
   if(hand.retain = 0) then cpHandleFree(hand);
end;

function cpHandleSetEql(obj : pointer; elt:pointer) : boolean;
var hand : pcpHandle;
begin
   hand := pcpHandle(elt);
   result:= (obj = hand.obj);
end;

function cphandleSetTrans(obj:pointer; unused:pointer) : pointer;
var hand :pcpHandle;
begin
   hand := cpHandleNew(obj);
   cpHandleRetain(hand);
   result:=hand;
end;

function cpSpaceHashInit( hash : PcpSpaceHash; celldim : cpFloat; numcells : integer; bbfunc : TcpSpaceHashBBFunc) : PcpSpaceHash;
begin
   cpSpaceHashAllocTable(hash, next_prime(numcells));
   hash.celldim := celldim;
   hash.bbfunc := bbfunc;

   hash.bins := nil;
   hash.handleSet := cpHashSetNew(0, cphandleSetEql, cphandleSetTrans);

   hash.stamp := 1;

   result:=hash;
end;

function cpSpaceHashNew( celldim : cpFloat; cells : integer; bbfunc : TcpSpaceHashBBFunc) : PcpSpaceHash;
begin
   result:=cpSpaceHashInit(cpSpaceHashAlloc,celldim,cells,bbfunc);
end;

procedure cphandleFreeWrap(elt:pointer; unused:pointer);
begin
   cpHandleFree(pcpHandle(elt));
end;

procedure cpClearHashCell(hash:pcpSpaceHash; index:integer);
var bin,next :pcpSpaceHashBin;
begin
   bin := hash.table[index];
   while assigned(bin) do begin
      next := bin.next;

      // Release the lock on the handle.
      cpHandleRelease(bin.handle);
      // Recycle the bin.
      bin.next := hash.bins;
      hash.bins := bin;

      bin := next;
   end;

   hash.table[index] := nil;
end;

procedure cpClearHash(hash:pcpSpaceHash);
var i : integer;
begin
   for i:=0 to hash.numcells-1 do cpclearHashCell(hash, i);
end;

procedure cpfreeBins(hash:pcpSpaceHash);
var bin,next:pcpSpaceHashBin;
begin
   bin := hash.bins;
   while assigned(bin) do begin
      next := bin.next;
      cfree(bin);
      bin := next;
   end;
end;

procedure cpSpaceHashDestroy( hash : PcpSpaceHash);
begin
   cpclearHash(hash);
   cpfreeBins(hash);

   // Free the handles.
   cpHashSetEach(hash.handleSet, cphandleFreeWrap, nil);
   cpHashSetFree(hash.handleSet);

   cfree(hash.table);
end;

procedure cpSpaceHashFree( hash : PcpSpaceHash);
begin
   if not assigned(hash) then exit;
   cpSpaceHashDestroy(hash);
   cfree(hash);
end;

procedure cpSpaceHashResize( hash : PcpSpaceHash; const celldim : cpFloat; const numcells: Integer);
begin
   // Clear the hash to release the old handle locks.
   cpclearHash(hash);

   hash.celldim := celldim;

   cpSpaceHashAllocTable(hash, next_prime(numcells));
end;

// Return true if the chain contains the handle.
function cpContainsHandle(bin:pcpSpaceHashBin; hand:pcpHandle):integer;
begin
   result:=1;

   while assigned(bin) do begin
      if(bin.handle = hand) then exit;
      bin := bin.next;
   end;

   result:=0;
end;

// Get a recycled or new bin.
function getEmptyBin(hash:pcpSpaceHash) :pcpSpaceHashBin;
var bin:pcpSpaceHashBin;
begin
   bin := hash.bins;
   // Make a new one if necessary.
   if(bin = nil) then begin
      result:=calloc(sizeof(result^));
      exit;
   end;

   hash.bins := bin.next;
   result:=bin;
end;

// The hash function itself.
function cphash_func(x,y,n:LongWord) : LongWord;
begin
   result:=(x*2185031351 xor y*4232417593) mod n;
end;

procedure cphashHandle(hash:pcpSpaceHash; hand:pcpHandle; bb:cpBB);
var dim : cpFloat;
    index,n,i,j : integer;
    newbin,bin:pcpSpaceHashBin;
    l,r,b,t : integer;
begin
   // Find the dimensions in cell coordinates.
   dim := hash.celldim;
   l := trunc(bb.l/dim);
   r := trunc(bb.r/dim);
   b := trunc(bb.b/dim);
   t := trunc(bb.t/dim);

   n := hash.numcells;
   for i:=l to r do begin
      for j:=b to t do begin
         index := cphash_func(i,j,n);
         bin := hash.table[index];

         // Don't add an object twice to the same cell.
         if(cpContainsHandle(bin, hand)<>0) then continue;

         cpHandleRetain(hand);
         // Insert a new bin for the handle in this cell.
         newBin := getEmptyBin(hash);
         newBin.handle := hand;
         newBin.next := bin;
         hash.table[index] := newBin;
      end;
   end;
end;

// Add an object to the hash.
procedure cpSpaceHashInsert( hash : PcpSpaceHash; obj : pointer; id : LongWord; bb : cpBB);
var hand : pcpHandle;
begin
   hand := pcpHandle(cpHashSetInsert(hash.handleSet, id, obj, nil));
   cphashHandle(hash, hand, bb);
end;

// Rehash only a specific object.
procedure cpSpaceHashRehashObject( hash : PcpSpaceHash; obj : pointer; id : LongWord);
var hand :pcpHandle;
begin
   hand := pcpHandle(cpHashSetFind(hash.handleSet, id, obj));
   cphashHandle(hash, hand, hash.bbfunc(obj));
end;

procedure cpHandleRehashHelper(elt:pointer;data:pointer);
var hand :pcpHandle;
    hash:pcpSpaceHash;
begin
   hand := pcpHandle(elt);
   hash := pcpSpaceHash(data);
   cphashHandle(hash, hand, hash.bbfunc(hand.obj));
end;

// Rehash the contents of the hash.
procedure cpSpaceHashRehash( hash : PcpSpaceHash);
begin
   cpClearHash(hash);
   // Rehash all of the handles.
   cpHashSetEach(hash.handleSet, cphandleRehashHelper, hash);
end;

// Remove an object from the hash.
procedure cpSpaceHashRemove( hash : PcpSpaceHash; obj : pointer; id : LongWord);
var hand :pcpHandle;
begin
   hand := pcpHandle(cpHashSetRemove(hash.handleSet, id, obj));
   if assigned(hand) then begin
      hand.obj := nil;
      cpHandleRelease(hand);
   end;
end;

procedure cpEachHelper(elt,data:pointer);{$IFDEF USE_INLINE}inline;{$ENDIF}
var pair:pcpEachPair;
    hand:pcpHandle;
begin
   hand:=elt;
   pair:=data;
   pair.func(hand.obj,pair.data);
end;

procedure cpSpaceHashEach( hash : PcpSpaceHash; func : TcpSpaceHashIterator; data : pointer);
var pair : cpeachPair;
begin
   // Bundle the callback up to send to the hashset iterator.
   pair.func:=func;
   pair.data:=data;
   cpHashSetEach(hash.handleSet, cpeachHelper, @pair);
end;

procedure cpSpaceHashQueryHelper(hash:pcpSpaceHash; bin:pcpSpaceHashBin; obj:pointer; func:TcpSpaceHashQueryFunc; data:pointer); //{$IFDEF USE_INLINE}inline;{$ENDIF}
var hand :pcpHandle;
    other :pointer;
begin
   while assigned(bin) do begin
      hand := bin.handle;
      other := hand.obj;

      // Skip over certain conditions
      if(
      // Have we already tried this pair in this query?
      (hand.stamp = hash.stamp)
      // Is obj the same as other?
      or (obj = other)
      // Has other been removed since the last rehash?
      or (not assigned(other))
      ) then begin
         bin := bin.next;
         continue;
      end;

      func(obj, other, data);

      // Stamp that the handle was checked already against this object.
      hand.stamp := hash.stamp;
      bin := bin.next;
   end;
end;

procedure cpSpaceHashPointQuery(hash:PcpSpaceHash; point:cpVect; func : TcpSpaceHashQueryFunc; data : pointer);
var dim : cpFloat;
    index : integer;
begin
  dim := hash.celldim;
  index := cphash_func(trunc(point.x/dim), trunc(point.y/dim), hash.numcells);

  cpSpaceHashQueryHelper(hash, hash.table[index], @point, func, data);

  // Increment the stamp.
  // Only one cell is checked, but query() requires it anyway.
   inc(hash.stamp);
end;

// Query the hash for a given BBox.
procedure cpSpaceHashQuery( hash : PcpSpaceHash; obj : pointer; const bb : cpBB; func : TcpSpaceHashQueryFunc; data : pointer);
var dim:cpFloat;
    index,l,r,b,t,n,i,j : integer;
begin
   // Get the dimensions in cell coordinates.
   dim := hash.celldim;
   l := trunc(bb.l/dim);
   r := trunc(bb.r/dim);
   b := trunc(bb.b/dim);
   t := trunc(bb.t/dim);

   n := hash.numcells;

   // Iterate over the cells and query them.
   for i:=l to r do begin
      for j:=b to t do begin
         index := cphash_func(i,j,n);
         cpSpaceHashQueryHelper(hash, hash.table[index], obj, func, data);
      end;
   end;
   // Increment the stamp.
   inc(hash.stamp);
end;

// Hashset iterator func used with cpSpaceHashQueryRehash().

procedure cpHandleQueryRehashHelper(elt:pointer;data:pointer);
  label break_out;
  var
    hand :pcpHandle;
    index,n,l,r,b,t,i,j : integer;
    obj : pointer;
    bb : cpBB;
    pair : pcpqueryRehashPair;
    hash : pcpSpaceHash;
    func : TcpSpaceHashQueryFunc;
    bin,newBin:pcpSpaceHashBin;
    dim : cpFloat;
begin
  hand := pcpHandle(elt);

  // Unpack the user callback data.
  pair := pcpqueryRehashPair(data);
  hash := pair.hash;
  func := pair.func;

  dim := hash.celldim;
  n := hash.numcells;

  obj := hand.obj;
  bb := hash.bbfunc(obj);

  l := trunc(bb.l/dim);
  r := trunc(bb.r/dim);
  b := trunc(bb.b/dim);
  t := trunc(bb.t/dim);

  for i:=l to r do
    for j:=b to t do
      begin
//        // exit the loops if the object has been deleted in func().
//        if not Assigned(hand.obj) Then
//          goto break_out;

        index := cphash_func(i,j,n);
        bin := hash.table[index];

        if(cpContainsHandle(bin, hand)<>0) then continue;
        cpHandleRetain(hand); // this MUST be done first in case the object is removed in func()
        cpSpaceHashQueryHelper(hash, bin, obj, func, pair.data);

        newBin := getEmptyBin(hash);
        newBin.handle := hand;
        newBin.next := bin;
        hash.table[index] := newBin;
      end;

//   break_out:
   // Increment the stamp for each object we hash.
   inc(hash.stamp);
end;

procedure cpSpaceHashQueryRehash( hash : PcpSpaceHash; func : TcpSpaceHashQueryFunc; data : pointer);
var pair : cpQueryRehashPair;
begin
   cpClearHash(hash);

   pair.hash := hash;
   pair.func := func;
   pair.data := data;

   cpHashSetEach(hash.handleSet, cpHandleQueryRehashHelper, @pair);
end;

// *****************************************************************************************************************************
//
//  Shape functions
//
// *****************************************************************************************************************************

procedure cpResetShapeIdCounter;
begin
   SHAPE_ID_COUNTER:=0;
end;

function cpShapeInit( shape : PcpShape; klass:PcpShapeClass; body : PcpBody) : PcpShape;
begin
   assert(assigned(body));

   inc(NumShapes);

   shape.klass:=klass;
   shape.id := SHAPE_ID_COUNTER;
   inc(SHAPE_ID_COUNTER);

   shape.body := body;
   shape.is_static:=(body.m=INFINITY) and (body.i=INFINITY);

   shape.e := 0.0;
   shape.u := 0.0;
   shape.surface_v := cpvzero;

   shape.collision_type := 0;
   shape.group := 0;
   shape.layers := $FFFF;

   shape.data := nil;

   cpShapeCacheBB(shape);

   cpArrayPush(body.shapes,shape);

   result:=shape;
end;

procedure cpShapeDestroy( shape : PcpShape);
begin
   if assigned(shape.body) then cpArrayDeleteObj(shape.body.shapes,shape);
   if assigned(shape.klass.destroy) then shape.klass.destroy(shape);
end;

procedure cpShapeFree( shape : PcpShape);
begin
   if assigned(shape) then cpShapeDestroy(shape);
   cfree(shape);
   dec(NumShapes);
end;

function cpShapeCacheBB( shape : PcpShape) : cpBB;
begin
   shape.bb:=shape.klass.cacheData(shape, shape.body.p, shape.body.rot);
   result:=shape.bb;
end;

function cpShapePointQuery(shape:PcpShape; p:cpVect) : integer;
begin
  result:=shape.klass.pointQuery(shape, p);
end;

function bbFromCircle(const c:cpVect; const r:cpFloat ) :cpBB; {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
   result:=cpBBNew(c.x-r, c.y-r, c.x+r, c.y+r);
end;

function cpCircleShapeCacheData(shape : PcpShape; const p,rot : cpVect):cpBB;
var circle : pcpCircleShape;
begin
   circle := pcpCircleShape(shape);
   circle.tc := cpvadd(p, cpvrotate(circle.c, rot));
   result:=bbFromCircle(circle.tc, circle.r);
end;

function cpCircleShapeAlloc : PcpCircleShape;
begin
   result:=calloc(sizeof(result^));
end;

function cpCircleShapePointQuery(shape:PcpShape; const p:cpVect) : integer;
var distSQ : cpFloat;
    circle:PcpCircleShape;
begin
   circle := PcpCircleShape(shape);
  distSQ := cpvlengthsq(cpvsub(circle.tc, p));
  if distSQ <= (circle.r*circle.r) then result:=1 else result:=0;
end;

function cpCircleShapeInit( circle : PcpCircleShape; body : PcpBody; radius : cpFloat; offset : cpVect) : PcpCircleShape;
begin
   circle.c := offset;
   circle.r := radius;

   cpShapeInit(pcpShape(circle),@circleClass,body);
   result:=circle;
end;

function cpCircleShapeNew( body : PcpBody; radius : cpFloat; offset : cpVect) : PcpShape;
begin
   result:=PcpShape(cpCircleShapeInit(cpCircleShapeAlloc,body,radius,offset));
   result.offset:=offset;
end;

function cpSegmentShapeCacheData(shape:pcpShape; const p,rot:cpVect):cpBB;
var seg : pcpSegmentShape;
    rad,l,r,s,t :cpFloat;
begin
   seg := pcpSegmentShape(shape);

   seg.ta := cpvadd(p, cpvrotate(seg.a, rot));
   seg.tb := cpvadd(p, cpvrotate(seg.b, rot));
   seg.tn := cpvrotate(seg.n, rot);

   if(seg.ta.x < seg.tb.x) then begin
      l := seg.ta.x;
      r := seg.tb.x;
   end else begin
      l := seg.tb.x;
      r := seg.ta.x;
   end;

   if(seg.ta.y < seg.tb.y) then begin
      s := seg.ta.y;
      t := seg.tb.y;
   end else begin
      s := seg.tb.y;
      t := seg.ta.y;
   end;

   rad := seg.r;
   result:=cpBBNew(l - rad, s - rad, r + rad, t + rad);
end;

function cpSegmentShapePointQuery(shape:PcpShape; const p:cpVect) : integer;
var seg : PcpSegmentShape;
    dn,dist,dt,dtMin,dtMax : cpFloat;
begin
   result:=0;
  seg := PcpSegmentShape(shape);

  // Calculate normal distance from segment.
  dn := cpvdot(seg.tn, p) - cpvdot(seg.ta, seg.tn);
  dist := abs(dn) - seg.r;
  if(dist > 0.0) then exit;

  // Calculate tangential distance along segment.
  dt := -cpvcross(seg.tn, p);
  dtMin := -cpvcross(seg.tn, seg.ta);
  dtMax := -cpvcross(seg.tn, seg.tb);

  // Decision tree to decide which feature of the segment to collide with.
  if(dt <= dtMin) then begin
    if(dt < (dtMin - seg.r)) then begin
      exit;
    end else begin
      if cpvlengthsq(cpvsub(seg.ta, p)) < (seg.r*seg.r) then result:=1 else result:=0;
         exit;
    end;
  end else begin
    if(dt < dtMax) then begin
      result:=1;
         exit;
    end else begin
      if(dt < (dtMax + seg.r)) then begin
        if cpvlengthsq(cpvsub(seg.tb, p)) < (seg.r*seg.r) then result:=1 else result:=0;
            exit;
      end else begin
            exit;
      end;
    end;
  end;

  result:=1;
end;

function cpSegmentShapeAlloc : PcpSegmentShape;
begin
   result:=calloc(sizeof(result^));
end;

function cpSegmentShapeInit( seg : PcpSegmentShape; body : PcpBody; a,b : cpVect; r : cpFloat) : PcpSegmentShape;
begin
   seg.a := a;
   seg.b := b;
   seg.n := cpvperp(cpvnormalize(cpvsub(b, a)));
   seg.r := r;

   cpShapeInit(PcpShape(seg),@segmentClass,body);

   result:=seg;
end;

function cpSegmentShapeNew( body : PcpBody; a,b : cpVect; r : cpFloat) : PcpShape;
begin
   result:=PcpShape(cpSegmentShapeInit(cpSegmentShapeAlloc,body,a,b,r));
end;

// Unsafe API (chipmunk_unsafe.h)

procedure cpCircleShapeSetRadius( shape: PcpShape; radius: Float );
begin
  assert( shape.klass = @circleClass );
  PcpCircleShape(shape).r := radius;
end;

procedure cpCircleShapeSetCenter( shape: PcpShape; center: cpVect );
begin
  assert( shape.klass = @circleClass );
  PcpCircleShape(shape).c := center;
end;

procedure cpSegmentShapeSetEndpoints( shape: PcpShape; a,b : cpVect );
begin
  assert(shape.klass = @segmentClass);
  PcpSegmentShape(shape).a := a;
  PcpSegmentShape(shape).b := b;
  PcpSegmentShape(shape).n := cpvperp(cpvnormalize(cpvsub(b, a)));
end;

procedure cpSegmentShapeSetRadius( shape: PcpShape; radius: Float );
begin
  assert(shape.klass = @segmentClass);
  PcpSegmentShape(shape).r := radius;
end;

procedure cpPolyShapeTransformVerts(poly:pcpPolyShape; p,rot:cpVect);
var src,dst:PcpVectArray;
    i : integer;
begin
   src := poly.verts;
   dst := poly.tverts;
   for i:=0 to poly.numVerts-1 do
      dst[i] := cpvadd(p, cpvrotate(src[i], rot));
end;

procedure cpPolyShapeTransformAxes(poly:pcpPolyShape; p:cpVect; rot:cpVect);
var src,dst:pcpPolyShapeAxisArray;
    n:cpVect;
    i : integer;
begin
   src := poly.axes;
   dst := poly.tAxes;
   for i:=0 to poly.numVerts-1 do begin
      n := cpvrotate(src[i].n, rot);
      dst[i].n := n;
      dst[i].d := cpvdot(p, n) + src[i].d;
   end;
end;

function cpPolyShapeCacheData(shape:pcpShape; const p,rot:cpVect) :cpBB;
var v:cpVect;
    poly :pcpPolyShape;
    l, b, r, t :cpFloat;
    verts:PcpVectArray;
    i : integer;
begin
   poly := pcpPolyShape(shape);

   cpPolyShapeTransformAxes(poly, p, rot);
   cpPolyShapeTransformVerts(poly, p, rot);

   verts := poly.tVerts;
   r := verts[0].x;
   l :=r;
   t := verts[0].y;
   b := t;

   for i:=1 to poly.numVerts-1 do begin
      v := verts[i];

      l := cpfmin(l, v.x);
      r := cpfmax(r, v.x);

      b := cpfmin(b, v.y);
      t := cpfmax(t, v.y);
   end;

   result:=cpBBNew(l, b, r, t);
end;

procedure cpPolyShapeDestroy(shape:pcpShape);
var poly:pcpPolyShape;
begin
   poly := pcpPolyShape(shape);

   cfree(poly.verts);
   cfree(poly.tVerts);

   cfree(poly.axes);
   cfree(poly.tAxes);
end;

function cpPolyShapePointQuery(shape:PcpShape; const p:cpVect) : integer;
begin
  // TODO Check against BB first?
  result:=cpPolyShapeContainsVert(PcpPolyShape(shape), p);
end;

function  cpPolyShapeAlloc : PcpPolyShape;
begin
   result:=calloc(sizeof(result^));
end;

function cpIsPolyConvex(verts : pcpVectArray; numVerts : integer) : boolean;
var vi,vj : cpvect;
    n : cpFloat;
    i,j : integer;
begin
   result:=False;
   if NumVerts<3 then exit;

   vi := cpvsub(verts[1],verts[0]);
   for i:= 1 to NumVerts-1 do begin
      j  := (i+1) mod NumVerts;
      vj := cpvsub(verts[j],verts[i]);
      n:=cpvcross(vi,vj);

      if (n > 0.0) then exit;
      vi:=vj;
   end;
   result:=True;
end;

procedure setUpVerts( poly : PcpPolyShape; numVerts : integer; verts : PcpVectArray; offset : cpVect; assumeConvex : boolean = true);
var i : integer;
    a,b,n : cpVect;
begin
   poly.numVerts := numVerts;

   poly.verts := calloc(numVerts,sizeof(cpVect));
   poly.tVerts:= calloc(numVerts,sizeof(cpVect));
   poly.axes  := calloc(numVerts,sizeof(cpPolyShapeAxis));
   poly.tAxes := calloc(numVerts,sizeof(cpPolyShapeAxis));

   for i:=0 to numVerts-1 do begin
      a := cpvadd(offset, verts[i]);
      b := cpvadd(offset, verts[(i+1) mod numVerts]);
      n := cpvnormalize(cpvperp(cpvsub(b, a)));

      poly.verts[i] := a;
      poly.axes[i].n := n;
      poly.axes[i].d := cpvdot(n, a);
   end;

   if assumeConvex then poly.convex:=true else poly.convex :=cpIsPolyConvex(poly.verts,poly.numVerts);
end;

function cpPolyShapeInit( poly : PcpPolyShape; body : PcpBody; numVerts : integer; verts : PcpVectArray; offset : cpVect; assumeConvex : boolean = true) : PcpPolyShape;
begin
   setUpVerts(poly, numVerts, verts, offset);
   cpShapeInit(PcpShape(poly),@polyClass,body);
   result:=poly;
end;

function cpPolyShapeNew( body : PcpBody; numVerts : integer; verts : PcpVectArray; offset : cpVect) : PcpShape;
begin
   result:=PcpShape(cpPolyShapeInit(cpPolyShapeAlloc,body,numVerts,verts,offset));
end;

// Unsafe API (chipmunk_unsafe.h)

procedure cpPolyShapeSetVerts( shape: PcpShape; numVerts: Integer; verts: PcpVectArray; offset: cpVect );
begin
  assert(shape.klass = @polyClass);
  cpPolyShapeDestroy(shape);
  setUpVerts(PcpPolyShape(shape), numVerts, verts, offset);
end;

function cpPolyShapeValueOnAxis( poly : PcpPolyShape; n : cpVect; d : cpFloat) : cpFloat;
var verts : PcpVectArray;
    min:cpFloat;
    i : integer;
begin
   verts := poly.tVerts;
   min := cpvdot(n, verts[0]);
   for i:=1 to poly.numVerts-1 do
      min := cpfmin(min, cpvdot(n, verts[i]));
   result:=min - d;
end;

function cpPolyShapeContainsVert( poly : PcpPolyShape; v : cpVect) : integer;
var axes:pcpPolyShapeAxisArray;
    i : integer;
    dist:cpFloat;
begin
   result:=0;
   axes := poly.tAxes;

   for i:=0 to poly.numVerts-1 do begin
      dist := cpvdot(axes[i].n, v) - axes[i].d;
      if(dist > 0.0) then exit;
   end;

   result:=1;
end;

// Same as cpPolyShapeContainsVert() but ignores faces pointing away from the normal.
function cpPolyShapeContainsVertPartial(poly : PcpPolyShape; v,n : cpVect) : integer;
var axes:pcpPolyShapeAxisArray;
    i : integer;
    dist:cpFloat;
begin
   result:=0;
   axes := poly.tAxes;

   for i:=0 to poly.numVerts-1 do begin
    if(cpvdot(axes[i].n, n) < 0.0) then continue;
    dist := cpvdot(axes[i].n, v) - axes[i].d;
      if(dist > 0.0) then exit;
  end;

  result:=1;
end;



// *****************************************************************************************************************************
//
//  Arbiter functions
//
// *****************************************************************************************************************************

function cpContactInit( con : PcpContact; p,n : cpVect; dist : cpFloat; hash : LongWord ) : PcpContact;
begin
   con.p := p;
   con.n := n;
   con.dist := dist;

   con.jnAcc := 0.0;
   con.jtAcc := 0.0;
   con.jBias := 0.0;

   con.hash := hash;

   result:=con;
end;

function cpContactsSumImpulses( contacts : PcpContactArray; numContacts :integer) : cpVect;
var i : integer;
    j:cpVect;
begin
   result := cpvzero;
   for i:=0 to numContacts-1 do begin
      j := cpvmult(contacts[i].n, contacts[i].jnAcc);
      result := cpvadd(result, j);
   end;
end;

function cpContactsSumImpulsesWithFriction( contacts : PcpContactArray; numContacts :integer) : cpVect;
var i : integer;
    j,t:cpVect;
begin
   result := cpvzero;
   for i:=0 to numContacts-1 do begin
      t := cpvperp(contacts[i].n);
      j := cpvadd(cpvmult(contacts[i].n, contacts[i].jnAcc), cpvmult(t, contacts[i].jtAcc));
      result := cpvadd(result, j);
   end;
end;

function cpArbiterAlloc : PcpArbiter;
begin
   result:=calloc(sizeof(result^));
end;

function cpArbiterInit( arb : PcpArbiter; a,b : PcpShape; stamp :integer) : PcpArbiter;
begin
   inc(NumArbiters);
   arb.numContacts := 0;
   arb.contacts := nil;

   arb.a := a;
   arb.b := b;

   arb.stamp := stamp;

   result:=arb;
end;

function cpArbiterNew( a,b : PcpShape; stamp :integer) : PcpArbiter;
begin
   result:=cpArbiterInit(cpArbiterAlloc,a,b,stamp);
end;
procedure cpArbiterDestroy( arb : PcpArbiter );
begin
   cfree(arb.contacts);
end;
procedure cpArbiterFree( arb : PcpArbiter );
begin
   dec(NumArbiters);
   if assigned(arb) then cpArbiterDestroy(Arb);
   cfree(arb);
end;

procedure cpFreeArbiters( space : pcpspace);
begin
   cpHashSetReject(space.contactSet, cpContactSetReject, space);
end;

procedure cpArbiterInject( arb : PcpArbiter; var contacts : PcpContactArray; numContacts :integer);
var i,j : integer;
    new_contact,old : pcpContact;
begin
    // Iterate over the possible pairs to look for hash value matches.
   for i:=0 to arb.numContacts-1 do begin
      old := @arb.contacts[i];
      for j:=0 to numContacts - 1 do begin
         new_contact := @contacts[j];
         // This could trigger false positives, but is fairly unlikely nor serious if it does.
         if(new_contact.hash = old.hash) then begin
            // Copy the persistant contact information.
            new_contact.jnAcc := old.jnAcc;
            new_contact.jtAcc := old.jtAcc;
         end;
      end;
   end;

   cfree(arb.contacts);

   arb.contacts := contacts;
   arb.numContacts := numContacts;
end;

procedure cpArbiterPreStep( arb : PcpArbiter; dt_inv : cpFloat );
var shapea,shapeb : pcpShape;
    a,b : pcpBody;
    i : integer;
    j,t,v1,v2:cpVect;
    con : pcpContact;
    e,kt,r1ct,r2ct,kn,r2cn,r1cn,mass_sum:cpFloat;
begin
   shapea := arb.a;
   shapeb := arb.b;

   e := shapea.e * shapeb.e;
   arb.u := shapea.u * shapeb.u;
   arb.target_v := cpvsub(shapeb.surface_v, shapea.surface_v);

   a := shapea.body;
   b := shapeb.body;

   for i:=0 to arb.numContacts-1 do begin
      con := @arb.contacts[i];

     // Calculate the offsets.
     con.r1 := cpvsub(con.p, a.p);
     con.r2 := cpvsub(con.p, b.p);

     // Calculate the mass normal.
     mass_sum := a.m_inv + b.m_inv;

     r1cn := cpvcross(con.r1, con.n);
     r2cn := cpvcross(con.r2, con.n);
     kn := mass_sum + a.i_inv*r1cn*r1cn + b.i_inv*r2cn*r2cn;
     con.nMass := 1.0/kn;

     // Calculate the mass tangent.
     t := cpvperp(con.n);
     r1ct := cpvcross(con.r1, t);
     r2ct := cpvcross(con.r2, t);
     kt := mass_sum + a.i_inv*r1ct*r1ct + b.i_inv*r2ct*r2ct;
     con.tMass := 1.0/kt;

     // Calculate the target bias velocity.
     con.bias := -cp_bias_coef*dt_inv*cpfmin(0.0, con.dist + cp_collision_slop);
     con.jBias := 0.0;

     // Calculate the target bounce velocity.
     v1 := cpvadd(a.v, cpvmult(cpvperp(con.r1), a.w));
     v2 := cpvadd(b.v, cpvmult(cpvperp(con.r2), b.w));
     con.bounce := cpvdot(con.n, cpvsub(v2, v1))*e;

      // Apply the previous accumulated impulse.
//      j := cpvadd(cpvmult(con.n, con.jnAcc), cpvmult(t, con.jtAcc));
//      cpBodyApplyImpulse(a, cpvneg(j), con.r1);
//      cpBodyApplyImpulse(b, j, con.r2);
   end;
end;

procedure cpArbiterApplyCachedImpulse(arb:PcpArbiter);
var shapea,shapeb : pcpShape;
    a,b : pcpBody;
    i : integer;
    con : pcpContact;
    j,t:cpVect;
begin
  shapea := arb.a;
  shapeb := arb.b;

  arb.u := shapea.u * shapeb.u;
  arb.target_v := cpvsub(shapeb.surface_v, shapea.surface_v);

  a := shapea.body;
  b := shapeb.body;

   for i:=0 to arb.numContacts-1 do begin
      con := @arb.contacts[i];
    t := cpvperp(con.n);
    j := cpvadd(cpvmult(con.n, con.jnAcc), cpvmult(t, con.jtAcc));
    cpBodyApplyImpulse(a, cpvneg(j), con.r1);
    cpBodyApplyImpulse(b, j, con.r2);
  end;
end;

procedure cpArbiterApplyImpulse( arb : PcpArbiter; eCoef : cpFloat );
var a,b : pcpbody;
    i : integer;
    con : pcpContact;
    j,t,v1,v2,vr,vb1,vb2,n,r1,r2,jb:cpVect;
    jt,jtMax,jtOld,vrt,jn,jnOld,vrn,jbn,jbnOld,vbn:cpFloat;
begin
   a := arb.a.body;
   b := arb.b.body;
   for i:=0 to arb.numContacts-1 do begin
      con := @arb.contacts[i];
      n := con.n;
      r1 := con.r1;
      r2 := con.r2;

      // Calculate the relative bias velocities.
      vb1 := cpvadd(a.v_bias, cpvmult(cpvperp(r1), a.w_bias));
      vb2 := cpvadd(b.v_bias, cpvmult(cpvperp(r2), b.w_bias));
      vbn := cpvdot(cpvsub(vb2, vb1), n);

      // Calculate and clamp the bias impulse.
      jbn := (con.bias - vbn)*con.nMass;
      jbnOld := con.jBias;
      con.jBias := cpfmax(jbnOld + jbn, 0.0);
      jbn := con.jBias - jbnOld;

      // Apply the bias impulse.
      jb := cpvmult(n, jbn);
      cpBodyApplyBiasImpulse(a, cpvneg(jb), r1);
      cpBodyApplyBiasImpulse(b, jb, r2);

      // Calculate the relative velocity.
      v1 := cpvadd(a.v, cpvmult(cpvperp(r1), a.w));
      v2 := cpvadd(b.v, cpvmult(cpvperp(r2), b.w));
      vr := cpvsub(v2, v1);
      vrn := cpvdot(vr, n);

      // Calculate and clamp the normal impulse.
      jn := -(con.bounce*eCoef + vrn)*con.nMass;
      jnOld := con.jnAcc;
      con.jnAcc := cpfmax(jnOld + jn, 0.0);
      jn := con.jnAcc - jnOld;

      // Calculate the relative tangent velocity.
      t := cpvperp(n);
      vrt := cpvdot(cpvadd(vr, arb.target_v), t);

      // Calculate and clamp the friction impulse.
      jtMax := arb.u*con.jnAcc;
      jt := -vrt*con.tMass;
      jtOld := con.jtAcc;
      con.jtAcc := cpfclamp(jtOld + jt, -jtMax, jtMax);
      jt := con.jtAcc - jtOld;

      // Apply the final impulse.
      j := cpvadd(cpvmult(n, jn), cpvmult(t, jt));
      cpBodyApplyImpulse(a, cpvneg(j), r1);
      cpBodyApplyImpulse(b, j, r2);
   end;
end;

// *****************************************************************************************************************************
//
// constraint functions
//
// *****************************************************************************************************************************

procedure cpConstraintDestroy( constraint : Pcpconstraint);
begin
// do nothing
end;

procedure cpConstraintFree( constraint : Pcpconstraint);
begin
   if not assigned(constraint) then exit;
   if constraint.klass.cpType=CP_BREAKABLE_JOINT then begin
      if pcpBreakableJoint(constraint).free_delegate then cpConstraintFree(pcpBreakableJoint(constraint).delegate);
   end;
   cpConstraintDestroy( constraint );
   cfree(constraint);
   dec(NumConstraints);
end;

procedure cpConstraintInit(constraint : PcpConstraint; klass: PcpConstraintClass; a,b: PcpBody);
begin
   inc(NumConstraints);
   constraint.klass:=klass;
   constraint.a:=a;
   constraint.b:=b;
   constraint.maxForce:=INFINITY;
   constraint.maxBias:=INFINITY;
   constraint.biasCoef:=cp_constraint_bias_coef;
end;

function relative_velocity(a,b:PcpBody; r1,r2:cpVect) :cpVect;
var v1_sum,v2_sum : cpVect;
begin
   v1_sum := cpvadd(a.v, cpvmult(cpvperp(r1), a.w));
  v2_sum := cpvadd(b.v, cpvmult(cpvperp(r2), b.w));
  result:=cpvsub(v2_sum, v1_sum);
end;

function normal_relative_velocity(a,b:PcpBody; r1,r2,n:cpVect) :cpFloat;
begin
   result:=cpvdot(relative_velocity(a,b,r1,r2),n);
end;

function mult_k(const vr,k1,k2:cpVect) : cpVect;
begin
   result:=cpv(cpvdot(vr,k1),cpvdot(vr,k2));
end;

function clamp_vect(const v:cpVect;const len : cpFloat) : cpVect;
begin
   if cpvdot(v,v)>len*len then result:=cpvmult(cpvnormalize(v),len) else result:=v;
end;

function k_scalar(a,b: PcpBody; const r1,r2,n:cpVect) : cpFloat;
var mass_sum:cpFloat;
    r1cn,r2cn:cpFloat;
begin
  mass_sum := a.m_inv + b.m_inv;
  r1cn := cpvcross(r1, n);
  r2cn := cpvcross(r2, n);

  result:=mass_sum + a.i_inv*r1cn*r1cn + b.i_inv*r2cn*r2cn;
end;

procedure k_tensor(a,b: pcpBody; const r1,r2:cpVect; var k1,k2:cpVect);
var r2nxy,r2xsq,r1xsq,r2ysq,r1nxy,r1ysq,a_i_inv,b_i_inv,m_sum,k11, k12, k21, k22,det_inv:cpFloat;
begin
  // calculate mass matrix
  // If I wasn't lazy and wrote a proper matrix class, this wouldn't be so gross...

  m_sum := a.m_inv + b.m_inv;

  // start with I*m_sum
  k11 := m_sum; k12 := 0.0;
  k21 := 0.0;  k22 := m_sum;

  // add the influence from r1
  a_i_inv := a.i_inv;
  r1xsq :=  r1.x * r1.x * a_i_inv;
  r1ysq :=  r1.y * r1.y * a_i_inv;
  r1nxy := -r1.x * r1.y * a_i_inv;
  k11 := k11+r1ysq; k12 := k12+r1nxy;
  k21 := k21+r1nxy; k22 := k22+r1xsq;

  // add the influnce from r2
  b_i_inv := b.i_inv;
  r2xsq :=  r2.x * r2.x * b_i_inv;
  r2ysq :=  r2.y * r2.y * b_i_inv;
  r2nxy := -r2.x * r2.y * b_i_inv;
  k11 := k11+r2ysq; k12 := k12+r2nxy;
  k21 := k21+r2nxy; k22 := k22+r2xsq;

  // invert
  det_inv := 1.0/(k11*k22 - k12*k21);
  k1 := cpv( k22*det_inv, -k12*det_inv);
  k2 := cpv(-k21*det_inv,  k11*det_inv);
end;

procedure apply_impulses(a,b: PcpBody; r1,r2,j:cpVect);
begin
  cpBodyApplyImpulse(a, cpvneg(j), r1);
  cpBodyApplyImpulse(b, j, r2);
end;

procedure apply_bias_impulses(a,b:PcpBody; r1,r2,j: cpVect);
begin
  cpBodyApplyBiasImpulse(a, cpvneg(j), r1);
  cpBodyApplyBiasImpulse(b, j, r2);
end;

function J_MAX(constraint:pointer;const dt:cpFloat):cpFloat;
begin
   result:=PcpConstraint(constraint).maxForce*dt;
end;

procedure breakableJointPreStep(constraint: PcpConstraint; const dt,dt_inv:cpFloat);
var delegate:PcpConstraint;
    jnt : PcpBreakableJoint;
begin
    jnt:=pointer(constraint);
    delegate:= jnt.delegate;

  if(delegate.klass.getImpulse(delegate)*jnt.last_dt_inv >= constraint.maxForce) then begin
    cpSpaceRemoveConstraint(constraint.space, constraint);
      cpConstraintFree(constraint);
    exit;
  end;

  delegate.klass.preStep(delegate, dt, dt_inv);
  jnt.last_dt_inv := dt_inv;
end;

procedure breakableJointApplyImpulse(constraint: PcpConstraint);
var delegate:PcpConstraint;
    jnt : PcpBreakableJoint;
begin
   jnt:=pointer(constraint);
   delegate:=jnt.delegate;
   delegate.klass.applyImpulse(delegate);
end;

function breakableJointGetImpulse(constraint: PcpConstraint) : cpFloat;
var delegate:PcpConstraint;
    jnt : PcpBreakableJoint;
begin
   jnt:=pointer(constraint);
   delegate:=jnt.delegate;
   result:=delegate.klass.getImpulse(delegate);
end;

function cpBreakableJointAlloc : PcpBreakableJoint;
begin
   result:=calloc(sizeof(result^));
end;

function cpBreakableJointInit(breakable:PcpBreakableJoint; delegate:PcpConstraint; space:PcpSpace; AfreeDelegate: boolean):PcpConstraint;
begin
  cpConstraintInit(PcpConstraint(breakable), @cpBreakableJointClass, nil, nil);

   breakable.free_delegate:=AfreeDelegate;
  breakable.delegate := delegate;

  result:=PcpConstraint(breakable);
end;

function cpBreakableJointNew(delegate:PcpConstraint; space:PcpSpace; AfreeDelegate: boolean):PcpConstraint;
begin
  result:=PcpConstraint(cpBreakableJointInit(cpBreakableJointAlloc, delegate, space, AfreeDelegate));
end;

function cpPinJointAlloc : PcpPinJoint;
begin
   result:=calloc(sizeof(result^));
end;

procedure pinJointPreStep(constraint:PcpConstraint; const dt,dt_inv:cpFloat);
var a,b : pcpbody;
   dist:cpFloat;
   j,delta:cpVect;
   jnt : PcpPinJoint;
begin
   a := constraint.a;
   b := constraint.b;
   jnt:=pointer(constraint);

   jnt.r1 := cpvrotate(jnt.anchr1, a.rot);
   jnt.r2 := cpvrotate(jnt.anchr2, b.rot);

   delta := cpvsub(cpvadd(b.p, jnt.r2), cpvadd(a.p, jnt.r1));
   dist := cpvlength(delta);
   if dist<>0 then jnt.n := cpvmult(delta, 1.0/dist)
      else jnt.n := cpvmult(delta, 1.0/INFINITY);

   jnt.nMass := 1.0/k_scalar(a, b, jnt.r1, jnt.r2, jnt.n);

   // calculate bias velocity
   jnt.bias := cpfclamp(-jnt.constraint.biasCoef*dt_inv*(dist - jnt.dist),-jnt.constraint.maxBias,jnt.constraint.maxBias);
   jnt.jnMax := J_MAX(jnt,dt);

   // apply accumulated impulse
   j := cpvmult(jnt.n, jnt.jnAcc);
  apply_impulses(a, b, jnt.r1, jnt.r2, j);
end;

procedure pinJointApplyImpulse(constraint:PcpConstraint);
var a,b:pcpBody;
    n,r1,r2:cpVect;
    jn,jnOld,vrn : cpFloat;
    jnt : PcpPinJoint;
begin
   a := constraint.a;
   b := constraint.b;
   jnt:=pointer(constraint);

   n := jnt.n;
   r1 := jnt.r1;
   r2 := jnt.r2;

   //calculate bias impulse
  vrn := normal_relative_velocity(a, b, jnt.r1, jnt.r2, n);

   // compute normal impulse
   jn := (jnt.bias-vrn)*jnt.nMass;
   jnOld:=jnt.jnAcc;
   jnt.jnAcc:=cpfclamp(jnOld + jn, -jnt.jnMax, jnt.jnMax);
   jn:=jnt.jnAcc-jnOld;

  apply_impulses(a, b, jnt.r1, jnt.r2, cpvmult(n, jn));
end;

function pinJointGetImpulse(constraint : PcpConstraint) : cpFloat;
begin
   result:=abs(PcpPinJoint(constraint).jnAcc);
end;

function cpPinJointReInit( joint : PcpPinJoint; a,b : PcpBody; anchr1,anchr2 : cpVect) : PcpPinJoint;
var s: pcpspace;
begin
   s:=joint.constraint.space;

   fillchar(joint^,sizeof(joint^),0);
   result:=cpPinJointInit(joint,a,b,anchr1,anchr2);
   result.constraint.space:=s;
end;

function cpPinJointInit( joint : PcpPinJoint; a,b : PcpBody; anchr1,anchr2 : cpVect) : PcpPinJoint;
var p1,p2:cpVect;
begin
   cpConstraintInit(PcpConstraint(joint),@cpPinJointClass,a,b);

   joint.anchr1 := anchr1;
   joint.anchr2 := anchr2;

   p1 := cpvadd(a.p, cpvrotate(anchr1, a.rot));
   p2 := cpvadd(b.p, cpvrotate(anchr2, b.rot));
   joint.dist := cpvdist(p2, p1);

   joint.jnAcc := 0.0;

   result:=joint;
end;

function cpPinJointInit(joint: PcpPinJoint; a,b: PcpBody; pivot : cpVect) : PcpPinJoint;
begin
   result:=cpPinJointInit( joint, a,b,cpvunrotate(cpvsub(pivot, a.p), a.rot),cpvunrotate(cpvsub(pivot, b.p), b.rot));
end;


function cpPinJointNew( a,b : PcpBody; anchr1,anchr2 : cpVect) : PcpConstraint;
begin
   result:=PcpConstraint(cpPinJointInit(cpPinJointAlloc,a,b,anchr1,anchr2));
end;

function cpPinJointNew(a,b: PcpBody; pivot : cpVect) : PcpConstraint;
begin
   result:=PcpConstraint(cpPinJointInit(cpPinJointAlloc,a,b,pivot));
end;

function cpDampedSpringAlloc : PcpDampedSpring;
begin
   result:=calloc(sizeof(result^));
end;

procedure DampedSpringPreStep(constraint:PcpConstraint; const dt,dt_inv:cpFloat);
var a,b : pcpbody;
   dist:cpFloat;
   delta:cpVect;
   jnt:PcpDampedSpring;
begin
   a := constraint.a;
   b := constraint.b;
   jnt:=pointer(constraint);

   jnt.r1 := cpvrotate(jnt.anchr1, a.rot);
   jnt.r2 := cpvrotate(jnt.anchr2, b.rot);

   delta := cpvsub(cpvadd(b.p, jnt.r2), cpvadd(a.p, jnt.r1));
   dist := cpvlength(delta);
   if dist<>0 then jnt.n := cpvmult(delta, 1.0/dist)
      else jnt.n := cpvmult(delta, 1.0/INFINITY);

   jnt.nMass := 1.0/k_scalar(a, b, jnt.r1, jnt.r2, jnt.n);

   jnt.dt:=dt;
   jnt.target_vrn:=0;

   jnt.f_spring := (jnt.restLength - dist)*jnt.stiffness;

  apply_impulses(a, b, jnt.r1, jnt.r2, cpvmult(jnt.n, jnt.f_spring*dt));
end;

procedure DampedSpringApplyImpulse(constraint:PcpConstraint);
var a,b:pcpBody;
    n,r1,r2:cpVect;
    v_damp,vrn : cpFloat;
   jnt:PcpDampedSpring;
begin
   a := constraint.a;
   b := constraint.b;
   jnt:=pointer(constraint);

   n := jnt.n;
   r1 := jnt.r1;
   r2 := jnt.r2;

   vrn := normal_relative_velocity(a, b, r1, r2, n) - jnt.target_vrn;

  // compute velocity loss from drag
  // not 100% certain this is derived correctly, though it makes sense
   v_damp := -vrn*(1.0 - exp(-jnt.damping*jnt.dt/jnt.nMass));
   jnt.target_vrn:=vrn+v_damp;

  apply_impulses(a, b, jnt.r1, jnt.r2, cpvmult(n, v_damp*jnt.nMass));
end;

function DampedSpringGetImpulse(constraint : PcpConstraint) : cpFloat;
begin
   result:=abs(pcpDampedSpring(constraint).f_spring/pcpDampedSpring(constraint).stiffness);
end;

function cpDampedSpringReInit( spring : PcpDampedSpring; a,b : PcpBody; anchr1,anchr2 : cpVect;restLength,stiffness,damping : cpFloat) : PcpDampedSpring;
begin
   fillchar(spring^,sizeof(spring^),0);
   result:=cpDampedSpringInit(spring,a,b,anchr1,anchr2,restLength,stiffness,damping);
end;

function cpDampedSpringInit( constraint : PcpDampedSpring; a,b : PcpBody; anchr1,anchr2 : cpVect;restLength,stiffness,damping : cpFloat) : PcpDampedSpring;
begin
   cpConstraintInit(pcpConstraint(constraint),@cpDampedSpringClass,a,b);

   constraint.anchr1 := anchr1;
   constraint.anchr2 := anchr2;

   if restlength<0 then constraint.restlength:=cpvDist(anchr1,anchr2)-restlength
      else constraint.restlength:=restlength;
   constraint.stiffness:=stiffness;
   constraint.damping:=damping;

   result:=constraint;
end;

function cpDampedSpringNew( a,b : PcpBody; anchr1,anchr2 : cpVect;restLength,stiffness,damping : cpFloat) : PcpConstraint;
begin
   result:=PcpConstraint(cpDampedSpringInit(cpDampedSpringAlloc,a,b,anchr1,anchr2,restLength,stiffness,damping));
end;

procedure cpDampedSpringSetProperties(spring:PcpDampedSpring; restLength,stiffness,damping : cpFloat);
begin
   spring.restlength:=restLength;
   spring.stiffness:=stiffness;
   spring.damping:=damping;
end;

procedure cpDampedSpringGetProperties(spring:PcpDampedSpring; var restLength,stiffness,damping : cpFloat);
begin
   restLength:=spring.restlength;
   stiffness:=spring.stiffness;
   damping:=spring.damping;
end;

function cpSlideJointAlloc : PcpSlideJoint;
begin
   result:=calloc(sizeof(result^));
end;

procedure SlideJointPreStep(constraint:PcpConstraint; const dt,dt_inv:cpFloat);
var a,b : pcpbody;
   pdist,dist:cpFloat;
   j,delta:cpVect;
   jnt:PcpSlideJoint;
begin
   a := constraint.a;
   b := constraint.b;
   jnt:=pointer(constraint);

   jnt.r1 := cpvrotate(jnt.anchr1, a.rot);
   jnt.r2 := cpvrotate(jnt.anchr2, b.rot);

   delta := cpvsub(cpvadd(b.p, jnt.r2), cpvadd(a.p, jnt.r1));
   dist := cpvlength(delta);
   pdist := 0.0;
   if(dist > jnt.max) then  begin
      pdist := dist - jnt.max;
   end else if(dist < jnt.min) then begin
      pdist := jnt.min - dist;
      dist := -dist;
   end;
   if dist<>0 then jnt.n := cpvmult(delta, 1.0/dist)
      else jnt.n := cpvmult(delta, 1.0/INFINITY);

  // calculate mass normal
  jnt.nMass := 1.0/k_scalar(a, b, jnt.r1, jnt.r2, jnt.n);

   // calculate bias velocity
   jnt.bias := cpfclamp(-jnt.constraint.biasCoef*dt_inv*pdist,-jnt.constraint.maxBias,jnt.constraint.maxBias);
   jnt.jnMax:=J_MAX(jnt,dt);

   // apply accumulated impulse
   if(jnt.bias=0) then jnt.jnAcc := 0.0;

   j := cpvmult(jnt.n, jnt.jnAcc);

   apply_impulses(a, b, jnt.r1, jnt.r2, j);
end;

procedure SlideJointApplyImpulse(constraint:PcpConstraint);
var a,b:pcpBody;
    vr,n,r1,r2:cpVect;
    jnOld,jn,vrn : cpFloat;
   jnt:PcpSlideJoint;
begin
   jnt:=pointer(constraint);

   if (jnt.bias=0) then exit;

   a := constraint.a;
   b := constraint.b;

   n := jnt.n;
   r1 := jnt.r1;
   r2 := jnt.r2;

   //calculate bias impulse
   vr := relative_velocity(a,b,r1,r2);
   vrn := cpvdot(vr, n);

   // compute normal impulse
   jn := (jnt.bias-vrn)*jnt.nMass;
   jnOld := jnt.jnAcc;
   jnt.jnAcc := cpfClamp(jnOld + jn,-jnt.jnMax, 0.0);
   jn := jnt.jnAcc - jnOld;

   // apply impulse
  apply_impulses(a, b, jnt.r1, jnt.r2, cpvmult(n, jn));
end;

function cpSlideJointReInit( joint : PcpSlideJoint; a,b : PcpBody; anchr1,anchr2 : cpVect; min,max : cpFloat) : PcpSlideJoint;
begin
   fillchar(joint^,sizeof(joint^),0);
   result:=cpSlideJointInit(joint,a,b,anchr1,anchr2,min,max);
end;

function SlideJointGetImpulse(constraint : PcpConstraint) : cpFloat;
begin
   result:=abs(PcpSlideJoint(constraint).jnAcc);
end;

function cpSlideJointInit( joint : PcpSlideJoint; a,b : PcpBody; anchr1,anchr2 : cpVect; min,max : cpFloat) : PcpSlideJoint;
begin
   cpConstraintInit(PcpConstraint(joint),@cpSlideJointClass,a,b);

   joint.anchr1 := anchr1;
   joint.anchr2 := anchr2;
   joint.min := min;
   joint.max := max;

   joint.jnAcc := 0.0;

   result := joint;
end;

function cpSlideJointNew( a : PcpBody; b : PcpBody; anchr1,anchr2 : cpVect; min,max : cpFloat) : PcpConstraint;
begin
   result:=PcpConstraint(cpSlideJointInit(cpSlideJointAlloc,a,b,anchr1,anchr2,min,max));
end;

function  cpPivotJointAlloc : PcpPivotJoint;
begin
   result:=calloc(sizeof(result^));
end;

procedure pivotJointPreStep(constraint:pcpConstraint; const dt,dt_inv:cpFloat);
var a,b : pcpbody;
   delta:cpVect;
   jnt : PcpPivotJoint;
begin
   a := constraint.a;
   b := constraint.b;
   jnt:=pointer(constraint);

  jnt.r1 := cpvrotate(jnt.anchr1, a.rot);
  jnt.r2 := cpvrotate(jnt.anchr2, b.rot);
   k_tensor(a,b,jnt.r1,jnt.r2,jnt.k1,jnt.k2);
   jnt.jMaxLen:=J_MAX(jnt,dt);

   // calculate bias velocity
   delta := cpvsub(cpvadd(b.p, jnt.r2), cpvadd(a.p, jnt.r1));
   jnt.bias := clamp_vect(cpvmult(delta, -jnt.constraint.biasCoef*dt_inv),jnt.constraint.maxBias);

   // apply accumulated impulse
  apply_impulses(a, b, jnt.r1, jnt.r2, jnt.jAcc);
end;

procedure pivotJointApplyImpulse(constraint:pcpConstraint);
var a,b:pcpBody;
    jOld,vr,j,r1,r2:cpVect;
   jnt : PcpPivotJoint;
begin
   a := constraint.a;
   b := constraint.b;
   jnt:=pointer(constraint);

   r1 := jnt.r1;
   r2 := jnt.r2;

   //calculate bias impulse
   vr := relative_velocity(a,b,r1,r2);

  // compute normal impulse
  j := mult_k(cpvsub(jnt.bias, vr), jnt.k1, jnt.k2);
  jOld := jnt.jAcc;
  jnt.jAcc := clamp_vect(cpvadd(jnt.jAcc, j), jnt.jMaxLen);
  j := cpvsub(jnt.jAcc, jOld);

   // apply impulse
   apply_impulses(a, b, jnt.r1, jnt.r2, j);
end;

function pivotJointGetImpulse(constraint : PcpConstraint) : cpFloat;
begin
   result:=cpvLength(pcpPivotJoint(constraint).jAcc);
end;

function cpPivotJointReInit( joint : PcpPivotJoint; a,b : PcpBody; anchr1,anchr2 : cpVect) : PcpPivotJoint;
begin
   fillchar(joint^,sizeof(Joint^),0);
   result:=cpPivotJointInit(joint,a,b,anchr1,anchr2);
end;

function cpPivotJointReInit( joint : PcpPivotJoint; a,b : PcpBody; pivot : cpVect) : PcpPivotJoint; overload;
begin
   fillchar(joint^,sizeof(Joint^),0);
   result:=cpPivotJointInit(joint,a,b,pivot);
end;


function cpPivotJointInit( joint : PcpPivotJoint; a,b : PcpBody; anchr1,anchr2 : cpVect) : PcpPivotJoint;
begin
   cpConstraintInit(pcpConstraint(joint),@cpPivotJointClass,a,b);

//   joint.anchr1 := cpvunrotate(cpvsub(pivot, a.p), a.rot);
//   joint.anchr2 := cpvunrotate(cpvsub(pivot, b.p), b.rot);

   joint.anchr1 := anchr1;
   joint.anchr2 := anchr2;

   joint.jAcc := cpvzero;

   result := joint;
end;

function cpPivotJointInit( joint : PcpPivotJoint; a,b : PcpBody; pivot : cpVect) : PcpPivotJoint;
begin
   cpConstraintInit(pcpConstraint(joint),@cpPivotJointClass,a,b);

   joint.anchr1 := cpvunrotate(cpvsub(pivot, a.p), a.rot);
   joint.anchr2 := cpvunrotate(cpvsub(pivot, b.p), b.rot);

   joint.jAcc := cpvzero;

   result := joint;
end;


function cpPivotJointNew2( a,b : PcpBody; anchr1,anchr2 : cpVect) : PcpConstraint;
begin
   result:=PcpConstraint(cpPivotJointInit(cpPivotJointAlloc,a,b,anchr1,anchr2))
end;

function cpPivotJointNew( a,b : PcpBody; pivot : cpVect) : PcpConstraint;
begin
  Result := cpPivotJointNew2( a, b, cpBodyWorld2Local( a, pivot ), cpBodyWorld2Local( b, pivot ) );
  // result:=PcpConstraint(cpPivotJointInit(cpPivotJointAlloc,a,b,pivot))
end;

function  cpGrooveJointAlloc : PcpGrooveJoint;
begin
   result:=calloc(sizeof(result^));
end;

procedure grooveJointPreStep(constraint:PcpConstraint; const dt,dt_inv:cpFloat);
var a,b : pcpbody;
   td,d:cpFloat;
   tb,ta,n,delta:cpVect;
   jnt:pcpGrooveJoint;
begin
   a := constraint.a;
   b := constraint.b;
   jnt:=pointer(constraint);

  // calculate endpoints in worldspace
  ta := cpBodyLocal2World(a, jnt.grv_a);
  tb := cpBodyLocal2World(a, jnt.grv_b);

  // calculate axis
  n := cpvrotate(jnt.grv_n, a.rot);
  d := cpvdot(ta, n);

  jnt.grv_tn := n;
  jnt.r2 := cpvrotate(jnt.anchr2, b.rot);

  // calculate tangential distance along the axis of r2
  td := cpvcross(cpvadd(b.p, jnt.r2), n);
  // calculate clamping factor and r2
  if(td <= cpvcross(ta, n)) then begin
    jnt.clamp := 1.0;
    jnt.r1 := cpvsub(ta, a.p);
  end else if(td >= cpvcross(tb, n)) then begin
    jnt.clamp := -1.0;
    jnt.r1 := cpvsub(tb, a.p);
  end else begin
    jnt.clamp := 0.0;
    jnt.r1 := cpvsub(cpvadd(cpvmult(cpvperp(n), -td), cpvmult(n, d)), a.p);
  end;

  // Calculate mass tensor
  k_tensor(a, b, jnt.r1, jnt.r2, jnt.k1, jnt.k2);

  // compute max impulse
  jnt.jMaxLen := J_MAX(jnt, dt);

  // calculate bias velocity
  delta := cpvsub(cpvadd(b.p, jnt.r2), cpvadd(a.p, jnt.r1));
  jnt.bias := clamp_vect(cpvmult(delta, -jnt.constraint.biasCoef*dt_inv),jnt.constraint.maxBias);

  apply_impulses(a, b, jnt.r1, jnt.r2, jnt.jAcc);
end;

function grooveConstrain(jnt:pcpGrooveJoint; j:cpVect):cpVect; {$IFDEF USE_INLINE}inline;{$ENDIF}
var n,jClamp : cpVect;
begin
   n := jnt.grv_tn;
  if (jnt.clamp*cpvcross(j, n) > 0.0) then jClamp := j else jClamp := cpvmult(n, cpvdot(j, n));
  result:=clamp_vect(jClamp, jnt.jMaxLen);
end;

procedure grooveJointApplyImpulse(constraint:PcpConstraint);
var a,b:pcpBody;
    vr,jOld,j,r1,r2:cpVect;
   jnt:pcpGrooveJoint;
begin
   a := constraint.a;
   b := constraint.b;
   jnt:=pointer(constraint);

   r1 := jnt.r1;
   r2 := jnt.r2;

   //calculate bias impulse
   vr := relative_velocity(a,b,r1,r2);


  j := mult_k(cpvsub(jnt.bias, vr), jnt.k1, jnt.k2);
  jOld := jnt.jAcc;
  jnt.jAcc := grooveConstrain(jnt, cpvadd(jOld, j));
  j := cpvsub(jnt.jAcc, jOld);

   // apply impulse
  apply_impulses(a, b, jnt.r1, jnt.r2, j);
end;

function grooveJointGetImpulse(constraint : PcpConstraint) : cpFloat;
begin
   result:=cpvlength(pcpGrooveJoint(constraint).jAcc);
end;

function cpGrooveJointReInit( joint : PcpGrooveJoint; a,b : PcpBody; groove_a,groove_b,anchr2 : cpVect) : PcpGrooveJoint;
begin
   fillchar(joint^,sizeof(Joint^),0);
   result:=cpGrooveJointInit(joint,a,b,groove_a,groove_b,anchr2);
end;

function cpGrooveJointInit( joint : PcpGrooveJoint; a,b : PcpBody; groove_a,groove_b,anchr2 : cpVect) : PcpGrooveJoint;
begin
   cpConstraintInit(pcpConstraint(joint),@cpGrooveJointClass,a,b);

   joint.grv_a := groove_a;
   joint.grv_b := groove_b;
   joint.grv_n := cpvperp(cpvnormalize(cpvsub(groove_b, groove_a)));
   joint.anchr2 := anchr2;

   joint.jAcc := cpvzero;

   result := joint;
end;

function cpGrooveJointNew( a,b : PcpBody; groove_a,groove_b,anchr2 : cpVect) : PcpConstraint;
begin
   result:=PcpConstraint(cpGrooveJointInit(cpGrooveJointAlloc,a,b,groove_a,groove_b,anchr2));
end;

function cpSimpleMotorAlloc : PcpSimpleMotor;
begin
   result:=calloc(sizeof(result^));
end;

function cpSimpleMotorInit(joint: PcpSimpleMotor; a,b : PcpBody; rate : cpFloat) : PcpSimpleMotor;
begin
   cpConstraintInit(PcpConstraint(joint), @cpSimpleMotorClass, a, b);

  joint.rate := rate;

  joint.jAcc := 0.0;

  result:=joint;
end;

function cpSimpleMotorNew(a,b: PcpBody; rate : cpFloat) : PcpConstraint;
begin
   result:=PcpConstraint(cpSimpleMotorInit(cpSimpleMotorAlloc,a,b,rate));
end;

procedure SimpleMotorPreStep(constraint: PcpConstraint; const dt,dt_inv:cpFloat);
var jnt:pcpSimpleMotor;
    a,b:pcpBody;
begin
   a := constraint.a;
   b := constraint.b;
   jnt:=pointer(constraint);

   // calculate moment of inertia coefficient.
   jnt.iSum:=1/(a.i_inv+b.i_inv);

   // compute max impulse
   jnt.jMax:=J_MAX(jnt,dt);

   // apply joint torque
   a.w:=a.w-jnt.jAcc*a.i_inv;
   b.w:=b.w+jnt.jAcc*b.i_inv;
end;

procedure SimpleMotorApplyImpulse(constraint: PcpConstraint);
var jnt:pcpSimpleMotor;
    a,b:pcpBody;
    wr,j,jOld:cpFloat;
begin
   a := constraint.a;
   b := constraint.b;
   jnt:=pointer(constraint);

  // compute relative rotational velocity
  wr := b.w - a.w + jnt.rate;

  // compute normal impulse
  j := -wr*jnt.iSum;
  jOld := jnt.jAcc;
  jnt.jAcc := cpfclamp(jOld + j, -jnt.jMax, jnt.jMax);
  j := jnt.jAcc - jOld;

  // apply impulse
  a.w := a.w-j*a.i_inv;
  b.w := b.w+j*b.i_inv;
end;

function SimpleMotorGetImpulse(constraint : PcpConstraint) : cpFloat;
begin
   result:=abs(pcpSimpleMotor(constraint).jAcc);
end;


function cpGearJointAlloc : PcpGearJoint;
begin
   result:=calloc(sizeof(result^));
end;

function cpGearJointInit(joint: PcpGearJoint; a,b : PcpBody; phase,ratio : cpFloat) : PcpGearJoint;
begin
  cpConstraintInit(PcpConstraint(joint), @cpGearJointClass, a, b);

  joint.phase := phase;
  joint.ratio := ratio;

  joint.jAcc := 0.0;

  result:=joint;
end;

function cpGearJointNew(a,b: PcpBody; phase,ratio : cpFloat) : PcpConstraint;
begin
   result:=PcpConstraint(cpGearJointInit(cpGearJointAlloc,a,b,phase,ratio));
end;

procedure GearJointPreStep(constraint: PcpConstraint; const dt,dt_inv:cpFloat);
var jnt:pcpGearJoint;
    a,b:pcpBody;
    maxBias:cpFloat;
begin
   a := constraint.a;
   b := constraint.b;
   jnt:=pointer(constraint);
  // calculate moment of inertia coefficient.
  jnt.iSum := 1.0/(a.i_inv*abs(jnt.ratio) + b.i_inv);

  // calculate bias velocity
   maxBias:=jnt.constraint.maxBias;
  jnt.bias := cpfclamp(-jnt.constraint.biasCoef*dt_inv*(b.a*jnt.ratio - a.a - jnt.phase),-maxBias,maxBias);

  // compute max impulse
  jnt.jMax := J_MAX(jnt, dt);

  // apply jnt torque
  a.w := a.w-jnt.jAcc*a.i_inv;
  b.w := b.w+jnt.jAcc*b.i_inv*jnt.ratio;
end;

procedure GearJointApplyImpulse(constraint: PcpConstraint);
var jnt:pcpGearJoint;
    a,b:pcpBody;
    j,jOld,wr:cpFloat;
begin
   a := constraint.a;
   b := constraint.b;
   jnt:=pointer(constraint);

  // compute relative rotational velocity
  wr := b.w*jnt.ratio - a.w;

  // compute normal impulse
  j := (jnt.bias - wr)*jnt.iSum;
  jOld := jnt.jAcc;
  jnt.jAcc := cpfclamp(jOld + j, -jnt.jMax, jnt.jMax);
  j := jnt.jAcc - jOld;

  // apply impulse
  a.w := a.w-j*a.i_inv;
  b.w := b.w+j*b.i_inv*jnt.ratio;
end;

function GearJointGetImpulse(constraint : PcpConstraint) : cpFloat;
begin
   result:=abs(pcpGearJoint(constraint).jAcc);
end;

function cpRotaryLimitJointAlloc : PcpRotaryLimitJoint;
begin
   result:=calloc(sizeof(result^));
end;

function cpRotaryLimitJointInit(joint: PcpRotaryLimitJoint; a,b : PcpBody; min,max : cpFloat) : PcpRotaryLimitJoint;
begin
  cpConstraintInit(pcpConstraint(joint), @cpRotaryLimitJointClass, a, b);

  joint.min := min;
  joint.max  := max;

  joint.jAcc := 0.0;

  result:=joint;
end;

function cpRotaryLimitJointNew(a,b: PcpBody; min,max : cpFloat) : PcpConstraint;
begin
   result:=PcpConstraint(cpRotaryLimitJointInit(cpRotaryLimitJointAlloc,a,b,min,max));
end;

procedure RotaryLimitJointPreStep(constraint: PcpConstraint; const dt,dt_inv:cpFloat);
var jnt:pcpRotaryLimitJoint;
    a,b:pcpBody;
    dist,pdist:cpFloat;
begin
   a := constraint.a;
   b := constraint.b;
   jnt:=pointer(constraint);

  dist := b.a - a.a;
  pdist := 0.0;
  if(dist > jnt.max) then begin
    pdist := jnt.max - dist;
  end else if(dist < jnt.min) then begin
    pdist := jnt.min - dist;
  end;

  // calculate moment of inertia coefficient.
  jnt.iSum := 1.0/(a.i_inv + b.i_inv);

  // calculate bias velocity
  jnt.bias := cpfclamp(-jnt.constraint.biasCoef*dt_inv*(pdist),-jnt.constraint.maxBias,jnt.constraint.maxBias);

  // compute max impulse
  jnt.jMax := J_MAX(jnt, dt);

  // If the bias is 0, the jnt is not at a limit. Reset the impulse.
  if(jnt.bias=0) then jnt.jAcc := 0.0;

  // apply jnt torque
  a.w := a.w-jnt.jAcc*a.i_inv;
  b.w := b.w+jnt.jAcc*b.i_inv;
end;

procedure RotaryLimitJointApplyImpulse(constraint: PcpConstraint);
var jnt:pcpRotaryLimitJoint;
    a,b:pcpBody;
    wr,j,jOld:cpFloat;

begin
   jnt:=pointer(constraint);
   if jnt.bias=0 then exit;
   a := constraint.a;
   b := constraint.b;

  // compute relative rotational velocity
  wr := b.w - a.w;

  // compute normal impulse
  j := -(jnt.bias + wr)*jnt.iSum;
  jOld := jnt.jAcc;
  if(jnt.bias < 0.0) then begin
    jnt.jAcc := cpfclamp(jOld + j, 0.0, jnt.jMax);
  end else begin
    jnt.jAcc := cpfclamp(jOld + j, -jnt.jMax, 0.0);
  end;
  j := jnt.jAcc - jOld;

  // apply impulse
  a.w := a.w-j*a.i_inv;
  b.w := b.w+j*b.i_inv;
end;

function RotaryLimitJointGetImpulse(constraint : PcpConstraint) : cpFloat;
begin
   result:=abs(pcpRotaryLimitJoint(constraint).jAcc);
end;

function cpDampedRotarySpringAlloc : PcpDampedRotarySpring;
begin
   result:=calloc(sizeof(result^));
end;

function cpDampedRotarySpringInit(joint: PcpDampedRotarySpring; a,b : PcpBody; restAngle,stiffness,damping : cpFloat) : PcpDampedRotarySpring;
begin
  cpConstraintInit(PcpConstraint(joint), @cpDampedRotarySpringClass, a, b);

  joint.restAngle := restAngle;
  joint.stiffness := stiffness;
  joint.damping := damping;

  result:=joint;
end;

function cpDampedRotarySpringNew(a,b: PcpBody; restAngle,stiffness,damping : cpFloat) : PcpConstraint;
begin
   result:=PcpConstraint(cpDampedRotarySpringInit(cpDampedRotarySpringAlloc,a,b,restAngle,stiffness,damping));
end;

procedure DampedRotarySpringPreStep(constraint: PcpConstraint; const dt,dt_inv:cpFloat);
var jnt:pcpDampedRotarySpring;
    a,b:pcpBody;
    j_jnt:cpFloat;
begin
   a := constraint.a;
   b := constraint.b;
   jnt:=pointer(constraint);

  jnt.iSum := 1.0/(a.i_inv + b.i_inv);

  jnt.dt := dt;
  jnt.target_wrn := 0.0;

  // apply jnt torque
  j_jnt := (a.a - b.a - jnt.restAngle)*jnt.stiffness*dt;
  a.w := a.w-j_jnt*a.i_inv;
  b.w := b.w+j_jnt*b.i_inv;
end;

procedure DampedRotarySpringApplyImpulse(constraint: PcpConstraint);
var jnt:pcpDampedRotarySpring;
    a,b:pcpBody;
    wrn,w_damp,j_damp : cpFloat;
begin
   a := constraint.a;
   b := constraint.b;
   jnt:=pointer(constraint);

  // compute relative velocity
  wrn := a.w - b.w;//normal_relative_velocity(a, b, r1, r2, n) - jnt.target_vrn;

  // compute velocity loss from drag
  // not 100% certain this is derived correctly, though it makes sense
  w_damp := wrn*(1.0 - exp(-jnt.damping*jnt.dt/jnt.iSum));
  jnt.target_wrn := wrn - w_damp;

  //apply_impulses(a, b, jnt.r1, jnt.r2, cpvmult(jnt.n, v_damp*jnt.nMass));
  j_damp := w_damp*jnt.iSum;
  a.w := a.w-j_damp*a.i_inv;
  b.w := b.w+j_damp*b.i_inv;
end;

function DampedRotarySpringGetImpulse(constraint : PcpConstraint) : cpFloat;
begin
   result:=0;
end;

// *****************************************************************************************************************************
//
// Space functions
//
// *****************************************************************************************************************************

procedure freeWrap(ptr : pointer; unused : pointer);
begin
   cfree(ptr);
end;
procedure shapeFreeWrap(ptr : pointer; unused : pointer);
begin
   cpShapeFree(pcpShape(ptr));
end;
procedure arbiterFreeWrap(ptr : pointer; unused : pointer);
begin
   cpArbiterFree(pcpArbiter(ptr));
end;
procedure bodyFreeWrap(ptr : pointer; unused : pointer);
begin
    cpBodyFree(pcpBody(ptr));
end;
procedure constraintFreeWrap(ptr : pointer; unused : pointer);
begin
   cpConstraintFree(PcpConstraint(ptr));
end;

function cpSpaceAlloc : PcpSpace;
begin
   result:=calloc(sizeof(result^));
end;

const
      DEFAULT_DIM_SIZE = 100.0;
      DEFAULT_COUNT = 1000;
      DEFAULT_ITERATIONS = 10;
      DEFAULT_ELASTIC_ITERATIONS = 0;

// Equal function for contactSet.
function contactSetEql(ptr : pointer; elt:pointer) : boolean;
var arb:pcpArbiter;
    a,b :pcpShape;
    shapes : PcpShapeArray;
begin
   shapes := PcpShapeArray(ptr);
   a := shapes^[0];
   b := shapes^[1];

   arb := pcpArbiter(elt);

   result:= ((a = arb.a) and (b = arb.b)) or ((b = arb.a) and (a = arb.b));
end;

// Transformation function for contactSet.
function contactSetTrans(ptr:pointer; data:pointer) : pointer;
var space:pcpSpace;
    shapes : PcpShapeArray;
begin
   shapes := PcpShapeArray(ptr);
   space := pcpSpace(data);
   result:=cpArbiterNew(shapes[0], shapes[1], space.stamp);
end;

// Equals function for collFuncSet.
function cpCollFuncSetEql(ptr : pointer; elt:pointer) : boolean;
var ids :pcpUnsignedIntArray;
    pair : PcpCollPairFunc;
    a,b : LongWord;
begin
   ids := pcpUnsignedIntArray(ptr);
   a := ids[0];
   b := ids[1];

   pair := pcpCollPairFunc(elt);

   result:= ((a = pair.a) and (b = pair.b)) or ((b = pair.a) and (a = pair.b));
end;

// Transformation function for collFuncSet.
function collFuncSetTrans(ptr:pointer; data:pointer) : pointer;
var ids :pcpUnsignedIntArray;
    funcData:pcpCollFuncData;
    pair : PcpCollPairFunc;
begin
   ids := pcpUnsignedIntArray(ptr);
   funcData := pcpCollFuncData(data);
   new(pair);
   pair.a := ids[0];
   pair.b := ids[1];
   pair.func := funcData.func;
   pair.data := funcData.data;

   result:=pair;
end;

// Default collision pair function.
function alwaysCollide(a,b:pcpShape; arr:pcpContactArray; numCon:integer; normal_coef:cpFloat; data:pointer) : integer;
begin
   result:=1;
end;

// BBfunc callback for the spatial hash.
function bbfunc(ptr:pointer) :cpBB;
begin
   result:=pcpShape(ptr).bb;
end;

function  cpSpaceInit( space : PcpSpace) : PcpSpace;
var pairFunc:cpCollPairFunc;
begin
   space.iterations := DEFAULT_ITERATIONS;
   space.elasticIterations := DEFAULT_ELASTIC_ITERATIONS;
   // space.sleepTicks = 300;

   space.gravity := cpvzero;
   space.damping := 1.0;

   space.stamp := 0;

   space.staticShapes := cpSpaceHashNew(DEFAULT_DIM_SIZE, DEFAULT_COUNT, bbfunc);
   space.activeShapes := cpSpaceHashNew(DEFAULT_DIM_SIZE, DEFAULT_COUNT, bbfunc);

   space.bodies := cpArrayNew(0);
   space.arbiters := cpArrayNew(0);

   space.contactSet := cpHashSetNew(0, contactSetEql, contactSetTrans);

   space.constraints := cpArrayNew(0);

   pairFunc.a:=0;
   pairFunc.b:=0;
   pairFunc.func:=alwaysCollide;
   pairFunc.data:=nil;

   space.defaultPairFunc := pairFunc;
   space.collFuncSet := cpHashSetNew(0, cpCollFuncSetEql, collFuncSetTrans);
   space.collFuncSet.default_value := @space.defaultPairFunc;

   result:=space;
end;

function cpSpaceNew : PcpSpace;
begin
   result:=cpSpaceInit(cpSpaceAlloc);
end;

procedure cpSpaceClearArbiters ( space : PcpSpace);
begin
   if assigned(space.contactSet) then
      cpHashSetEach(space.contactSet, arbiterFreeWrap, nil);

//   cpArrayFree(space.arbiters);
//   space.arbiters := cpArrayNew(0);
end;

procedure cpSpaceDestroy( space : PcpSpace);
begin
   cpSpaceHashFree(space.staticShapes);
   cpSpaceHashFree(space.activeShapes);

   cpArrayFree(space.bodies);
   cpArrayFree(space.constraints);

   if assigned(space.contactSet) then
      cpHashSetEach(space.contactSet, arbiterFreeWrap, nil);
   cpHashSetFree(space.contactSet);

   cpArrayFree(space.arbiters);

   if assigned(space.collFuncSet) then
      cpHashSetEach(space.collFuncSet, freeWrap, nil);

   cpHashSetFree(space.collFuncSet);
end;

procedure cpSpaceFree( space : PcpSpace);
begin
   if assigned(space) then cpSpaceDestroy(space);
   cfree(space);
end;

procedure cpSpaceFreeChildren( space : PcpSpace);
begin
   cpSpaceHashEach(space.staticShapes, shapeFreeWrap, nil);
   cpSpaceHashEach(space.activeShapes, shapeFreeWrap, nil);
   cpArrayEach(space.bodies, bodyFreeWrap, nil);
   cpArrayEach(space.constraints, constraintFreeWrap, nil);

   space.bodies.num:=0;
   space.constraints.num:=0;
   cpSpaceHashFree(space.staticShapes);
   cpSpaceHashFree(space.activeShapes);
   space.staticShapes := cpSpaceHashNew(DEFAULT_DIM_SIZE, DEFAULT_COUNT, bbfunc);
   space.activeShapes := cpSpaceHashNew(DEFAULT_DIM_SIZE, DEFAULT_COUNT, bbfunc);
end;

procedure cpSpaceAddCollisionPairFunc(space : PcpSpace; a,b : LongWord; func : TcpCollFunc; data : pointer);
var hash :LongWord;
    ids : array[0..1] of LongWord;
    funcdata:cpCollFuncData;
begin
   ids[0] := a;
   ids[1] := b;
   hash := CP_HASH_PAIR(a, b);
   // Remove any old function so the new one will get added.
   cpSpaceRemoveCollisionPairFunc(space, a, b);

   funcData.func := func;
   funcData.data := data;
   cpHashSetInsert(space.collFuncSet, hash, @ids, @funcData);
end;

procedure cpSpaceRemoveCollisionPairFunc(space : PcpSpace; a,b : LongWord);
var hash :LongWord;
    ids : array[0..1] of LongWord;
    old_pair:pcpCollPairFunc;
begin
   ids[0] := a;
   ids[1] := b;
   hash := CP_HASH_PAIR(a, b);
   old_pair := pcpCollPairFunc(cpHashSetRemove(space.collFuncSet, hash, @ids));
   cfree(old_pair);
end;

procedure cpSpaceSetDefaultCollisionPairFunc( space : PcpSpace; func : TcpCollFunc; data : pointer);
var pairFunc :cpCollPairFunc;
begin
   pairFunc.a:=0;
   pairFunc.b:=0;
   if assigned(func) then begin
      pairFunc.func:=func;
      pairFunc.data:=data;
   end else begin
      pairFunc.func:=alwaysCollide;
      pairFunc.data:=nil;
   end;
   space.defaultPairFunc := pairFunc;
end;

function cpContactSetReject(ptr : pointer; data : pointer) : integer;
var arb : pcpArbiter;
    space : pcpSpace;
begin
   result:=0;
   arb := pcpArbiter(ptr);
   space := pcpSpace(data);

   if((space.stamp - arb.stamp) > cp_contact_persistence) then begin
      cpArbiterFree(arb);
      exit;
   end;

   result:=1;
end;

procedure cpSpaceAddShape( space : PcpSpace; shape : PcpShape);
begin
   cpSpaceHashInsert(space.activeShapes, shape, shape.id, shape.bb);
   shape.space:=space;
end;

procedure cpSpaceAddStaticShape( space : PcpSpace; shape : PcpShape);
begin
   cpShapeCacheBB(shape);
   cpSpaceHashInsert(space.staticShapes, shape, shape.id, shape.bb);
   shape.space:=space;
end;

procedure cpSpaceAddBody( space : PcpSpace; body : PcpBody);
begin
   cpArrayPush(space.bodies, body);
   body.space:=space;
end;

procedure cpSpaceAddConstraint( space : PcpSpace; constraint : PcpConstraint);
begin
   cpArrayPush(space.constraints, constraint);
   constraint.space:=space;
end;

procedure shapeRemovalArbiterReject( space: PcpSpace; shape: PcpShape );
  var
    i, num: Integer;
    old_ary, new_ary: PcpArray;
    arb: PcpArbiter;
begin
  old_ary := space.arbiters;
  num := old_ary.num;

  if num = 0 Then exit;

  // make a new arbiters array and copy over all valid arbiters
  new_ary := cpArrayNew(num);

  for i := 0 to num - 1 do
    begin
      arb := PcpArbiter( old_ary.arr[i] );
      if ( arb.a <> shape ) and ( arb.b <> shape ) Then
        cpArrayPush( new_ary, arb );
    end;

  space.arbiters := new_ary;
  cpArrayFree( old_ary );
end;


procedure cpSpaceRemoveShape( space : PcpSpace; shape : PcpShape);
begin
   cpSpaceHashRemove(space.activeShapes, shape, shape.id);
   shapeRemovalArbiterReject( space, shape );
   shape.space:=nil;
end;

procedure cpSpaceRemoveStaticShape( space : PcpSpace; shape : PcpShape);
begin
   cpSpaceHashRemove(space.staticShapes, shape, shape.id);
   shapeRemovalArbiterReject( space, shape );
   shape.space:=nil;
end;

procedure cpSpaceRemoveBody( space : PcpSpace; body : PcpBody);
begin
   cpArrayDeleteObj(space.bodies, body);
   body.space:=nil;
end;

procedure cpSpaceRemoveConstraint( space : PcpSpace; constraint : PcpConstraint);
begin
   cpArrayDeleteObj(space.constraints, constraint);
   constraint.space:=nil;
end;

procedure cpSpaceEachBody( space : PcpSpace; func : TcpSpaceBodyIterator; data : pointer);
var bodies:pcpArray;
    i : integer;
begin
   bodies := space.bodies;
   for i:=0 to bodies.num-1 do
      func(pcpBody(bodies.arr[i]), data);
end;

procedure cpSpaceResizeStaticHash( space : PcpSpace; const dim : cpFloat; const count : Integer);
begin
   cpSpaceHashResize(space.staticShapes, dim, count);
   cpSpaceHashRehash(space.staticShapes);
end;

procedure cpSpaceResizeActiveHash( space : PcpSpace; const dim : cpFloat; const count : Integer);
begin
   cpSpaceHashResize(space.activeShapes, dim, count);
end;

procedure cpUpdateBBCache(ptr:pointer;unused:pointer);
begin
   cpShapeCacheBB(pcpShape(ptr));
end;

procedure cpSpaceRehashStatic( space : PcpSpace);
begin
   cpSpaceHashEach(space.staticShapes, cpUpdateBBCache, nil);
   cpSpaceHashRehash(space.staticShapes);
end;

procedure pointQueryHelper(point,obj,data : pointer);
var shape:PcpShape;
    pair:TcpPointQueryFuncPair;
begin
  shape := PcpShape(obj);
   pair := TcpPointQueryFuncPair(data^);

  if(cpShapePointQuery(shape, PcpVect(point)^)<>0) then pair.func(shape, pair.data);
end;

procedure pointQuery(hash:PcpSpaceHash; point: cpVect; func:cpSpacePointQueryFunc; var data : pointer);
var pair : TcpPointQueryFuncPair;
begin
  pair.func := func;
   pair.data:=data;
  cpSpaceHashPointQuery(hash, point, pointQueryHelper, @pair);
end;

procedure cpSpaceShapePointQuery(space:PcpSpace; point: cpVect; func:cpSpacePointQueryFunc; data : pointer);
begin
  pointQuery(space.activeShapes, point, func, data);
end;

procedure cpSpaceStaticShapePointQuery(space:PcpSpace; const point: cpVect; func:cpSpacePointQueryFunc; data : pointer);
begin
  pointQuery(space.staticShapes, point, func, data);
end;

function cpQueryReject(a:pcpShape; b:pcpShape) : integer;
begin
   if
   // BBoxes must overlap
   (cpBBintersects(a.bb, b.bb)=0)
   // Don't collide shapes attached to the same body.
   or (a.body = b.body)
   // Don't collide objects in the same non-zero group
   or (((a.group<>0) and (b.group<>0)) and (a.group = b.group))
   // Don't collide objects that don't share at least on layer.
   or ((a.layers and b.layers)=0)
      then result:=1 else result:=0;
end;

// Callback from the spatial hash.
// TODO: Refactor this into separate functions?
procedure cpQueryFunc(p1:pointer; p2:pointer; data:pointer);
var a,b,temp,pair_a,pair_b : pcpShape;
    space : pcpSpace;
    ids : array[0..1] of LongWord;
    hash : LongWord;
    pairfunc : pcpCollPairFunc;
    contacts:pcpContactArray;
    numContacts : integer;
    normal_coef:cpFloat;
    shape_Pair:cpShapePair;
    arb:pcpArbiter;
begin
   // Cast the generic pointers from the spatial hash back to usefull types
   a := p1;
   b := p2;
   space := data;
   assert((a.klass.cptype <10) and (b.klass.cptype<10));

   // Reject any of the simple cases
   if(cpQueryReject(a,b)<>0) then exit;

   // Shape 'a' should have the lower shape type. (required by cpCollideShapes() )
   if(a.klass.cptype > b.klass.cptype) then begin
      temp := a;
      a := b;
      b := temp;
   end;

//   // Find the collision pair function for the shapes.
//   ids[0] := a.collision_type;
//   ids[1] := b.collision_type;
//   hash := CP_HASH_PAIR(a.collision_type, b.collision_type);
//   pairFunc := pcpCollPairFunc(cpHashSetFind(space.collFuncSet, hash, @ids));
//   if not assigned(pairFunc.func) then exit; // A NULL pair function means don't collide at all.

   // Narrow-phase collision detection.
   contacts := nil;
   numContacts := cpCollideShapes(a, b, contacts);
   if(numContacts=0) then exit; // Shapes are not colliding.

//   // The collision pair function requires objects to be ordered by their collision types.
//   pair_a := a;
//   pair_b := b;
//   normal_coef := 1.0;
//
//   // Swap them if necessary.
//   if(pair_a.collision_type <> pairFunc.a) then begin
//      temp := pair_a;
//      pair_a := pair_b;
//      pair_b := temp;
//      normal_coef := -1.0;
//   end;
//
//   if(pairFunc.func(pair_a, pair_b, contacts, numContacts, normal_coef, pairFunc.data)<>0) then begin
      // The collision pair function OKed the collision. record the contact information.

      // Get an arbiter from space.contactSet for the two shapes.
      // This is where the persistant contact magic comes from.
      shape_pair[0] := a;
      shape_pair[1] := b;

      arb := pcpArbiter(cpHashSetInsert(space.contactSet, CP_HASH_PAIR(a, b), @shape_pair, space));

      // Timestamp the arbiter.
      arb.stamp := space.stamp;
      arb.a := a; arb.b := b; // TODO: Investigate why this is still necessary?

      // Inject the new contact points into the arbiter.
      cpArbiterInject(arb, contacts, numContacts);

      // Add the arbiter to the list of active arbiters.
      cpArrayPush(space.arbiters, arb);
//   end else begin
//   // The collision pair function rejected the collision.
//      cfree(contacts);
//   end;
end;

procedure filterArbiterByCallback(space: PcpSpace);
  var
    arbiters    : PcpArray;
    i, num      : Integer;
    ary         : array of PcpArbiter;
    arb         : PcpArbiter;
    temp, a, b  : PcpShape;
    normal_coef : cpFloat;
    ids         : array[ 0..1 ] of LongWord;
    hash        : LongWord;
    pairFunc    : PcpCollPairFunc;
begin
  arbiters := space.arbiters;
  num      := space.arbiters.num;

  // copy to the stack
  SetLength( ary, num );
  Move( space.arbiters.arr[ 0 ], ary[ 0 ], num * SizeOf( Pointer ) );

  for i := 0 to num - 1 do
    begin
      arb := ary[ i ];

      // The collision pair function requires objects to be ordered by their collision types.
      a := arb.a;
      b := arb.b;
      normal_coef := 1.0;

      // Find the collision pair function for the shapes.
      ids[ 0 ] := a.collision_type;
      ids[ 1 ] := b.collision_type;
      hash     := CP_HASH_PAIR( a.collision_type, b.collision_type );
      pairFunc := PcpCollPairFunc( cpHashSetFind( space.collFuncSet, hash, @ids[ 0 ] ) );
      if not Assigned( pairFunc.func ) Then continue; // A NULL pair function means don't collide at all.

      // Swap them if necessary.
      if a.collision_type <> pairFunc.a Then
        begin
          temp := a;
          a    := b;
          b    := temp;
          normal_coef := -1.0;
        end;

      if pairFunc.func( a, b, arb.contacts, arb.numContacts, normal_coef, pairFunc.data ) = 0 Then
        cpArrayDeleteObj( space.arbiters, arb );
  end;
end;

procedure cpActive2staticIter(ptr:pointer; data:pointer);
begin
   cpSpaceHashQuery(pcpSpace(data).staticShapes, pcpShape(ptr), pcpShape(ptr).bb, cpQueryFunc, pcpSpace(data));
end;

procedure cpSpaceStep( space : PcpSpace; const dt : cpFloat);
var damping,dt_inv:cpFloat;
    bodies,arbiters,constraints:pcpArray;
    body : pcpBody;
    constraint : PcpConstraint;
    i,j : integer;
begin
   if(dt=0) then exit; // prevents div by zero.
   dt_inv := 1.0/dt;

   bodies := space.bodies;
   constraints := space.constraints;

   // Empty the arbiter list.
   cpHashSetReject(space.contactSet, cpContactSetReject, space);
   space.arbiters.num := 0;

   // Integrate velocities.
   for i:=0 to bodies.num-1 do begin
      body:=bodies.arr[i];
      body.position_func(body, dt);
   end;

   // Pre-cache BBoxes and shape data.
   cpSpaceHashEach(space.activeShapes, cpUpdateBBCache, nil);

   // Collide!
   cpSpaceHashEach(space.activeShapes, cpActive2staticIter, space);
   cpSpaceHashQueryRehash(space.activeShapes, cpQueryFunc, space);

  // Filter arbiter list based on collision callbacks
  filterArbiterByCallback(space);

   // Prestep the arbiters.
   arbiters := space.arbiters;
   for i:=0 to arbiters.num-1 do
      cpArbiterPreStep(arbiters.arr[i], dt_inv);

   // Prestep the joints.
   i:=0;
   while i<constraints.num do begin
      j:=constraints.num;
      constraint := PcpConstraint(constraints.arr[i]);
      constraint.klass.preStep(constraint, dt, dt_inv);
      if j=constraints.num then  inc(i);
   end;

   // Run the impulse solver.
   for i:=0 to space.elasticIterations-1 do begin
      for j:=0 to arbiters.num-1 do
         cpArbiterApplyImpulse(arbiters.arr[j],1);
      for j:=0 to constraints.num-1 do begin
         constraint := PcpConstraint(constraints.arr[j]);
         constraint.klass.applyImpulse(constraint);
      end;
   end;

  // Integrate velocities.
  damping := power(1.0/space.damping, -dt);
  for i:=0 to bodies.num-1 do begin
    body := PcpBody(bodies.arr[i]);
    body.velocity_func(body, space.gravity, damping, dt);
  end;

  for i:=0 to arbiters.num-1 do
    cpArbiterApplyCachedImpulse(arbiters.arr[i]);

  // Run the impulse solver.
  for i:=0 to space.iterations-1 do begin
    for j:=0 to arbiters.num-1 do
      cpArbiterApplyImpulse(arbiters.arr[j], 0.0);

    for j:=0 to constraints.num-1 do begin
         constraint:=constraints.arr[j];
      constraint.klass.applyImpulse(constraint);
    end;
  end;
   inc(space.stamp);
end;

initialization
  cpInitChipmunk;
  cpResetShapeIdCounter;

finalization
  cpShutdownChipmunk;

end.
