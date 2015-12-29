%module(directors="1") swigbullet
#pragma SWIG nowarn=350,351,394,395


%{

#include <BulletSoftBody/btSoftBody.h>
#include <BulletSoftBody/btSoftBodyHelpers.h>
#include <BulletSoftBody/btSoftRigidDynamicsWorld.h>
#include <BulletSoftBody/btSoftBodyRigidBodyCollisionConfiguration.h>
#include <BulletSoftBody/btDefaultSoftBodySolver.h>
#include <btBulletDynamicsCommon.h>
  %}


%{
  typedef btSoftBody::Element Element;
  typedef btSoftBody::Material Material;
  typedef btSoftBody::Feature Feature;
  typedef btSoftBody::Config Config;
  typedef btSoftBody::Node Node;
  typedef btSoftBody::fCollision fCollision;
  typedef btRigidBody::btRigidBodyConstructionInfo btRigidBodyConstructionInfo;
  %}

%rename("length1") *::length;
%rename("btTransform0") btTransform::btTransform();

%ignore btSequentialImpulseConstraintSolver::getSSE4_1ConstraintRowSolverGeneric();
%ignore btSequentialImpulseConstraintSolver::getSSE4_1ConstraintRowSolverLowerLimit();
%ignore btSequentialImpulseConstraintSolver::getSSE2ConstraintRowSolverGeneric();
%ignore btSequentialImpulseConstraintSolver::getSSE2ConstraintRowSolverLowerLimit();

%ignore btVector3DataName;
%ignore btQuaternionDataName;
%ignore btCollisionObjectDataName;
%ignore btRigidBodyDataName;
%ignore btTypedConstraintDataName;
%ignore btPoint2PointConstraintDataName;
%ignore btHingeConstraintDataName;
%ignore btConeTwistConstraintDataName;
%ignore btGeneric6DofConstraintDataName;
%ignore btSliderConstraintDataName;
%ignore btGeneric6DofSpringConstraintDataName;
%ignore btSoftBodyDataName;

//btSoftBody nested classes
struct Element
{
  void*m_tag;// User data
Element() : m_tag(0) {}
};

struct fCollision {
  enum _ {
    RVSmask=0x000f,///Rigid versus soft mask
    SDF_RS=0x0001,///SDF based rigid vs soft
    CL_RS=0x0002, ///Cluster vs convex rigid vs soft
    SVSmask=0x0030,///Rigid versus soft mask
    VF_SS=0x0010,///Vertex vs face soft vs soft handling
    CL_SS=0x0020, ///Cluster vs cluster soft vs soft handling
    CL_SELF =0x0040, ///Cluster soft body self collision
    /* presets*/ 
    Default=SDF_RS,
    END
  };
};

struct Material : Element
{
  btScalarm_kLST;// Linear stiffness coefficient [0,1]
  btScalarm_kAST;// Area/Angular stiffness coefficient [0,1]
  btScalarm_kVST;// Volume stiffness coefficient [0,1]
  intm_flags;// Flags
};

struct Feature : Element
{
  Material*m_material;// Material
};

struct Config
{
  //eAeroModel::_aeromodel;// Aerodynamic model (default: V_Point)
  btScalarkVCF;// Velocities correction factor (Baumgarte)
  btScalarkDP;// Damping coefficient [0,1]
  btScalarkDG;// Drag coefficient [0,+inf]
  btScalarkLF;// Lift coefficient [0,+inf]
  btScalarkPR;// Pressure coefficient [-inf,+inf]
  btScalarkVC;// Volume conversation coefficient [0,+inf]
  btScalarkDF;// Dynamic friction coefficient [0,1]
  btScalarkMT;// Pose matching coefficient [0,1]
  btScalarkCHR;// Rigid contacts hardness [0,1]
  btScalarkKHR;// Kinetic contacts hardness [0,1]
  btScalarkSHR;// Soft contacts hardness [0,1]
  btScalarkAHR;// Anchors hardness [0,1]
  btScalarkSRHR_CL;// Soft vs rigid hardness [0,1] (cluster only)
  btScalarkSKHR_CL;// Soft vs kinetic hardness [0,1] (cluster only)
  btScalarkSSHR_CL;// Soft vs soft hardness [0,1] (cluster only)
  btScalarkSR_SPLT_CL;// Soft vs rigid impulse split [0,1] (cluster only)
  btScalarkSK_SPLT_CL;// Soft vs rigid impulse split [0,1] (cluster only)
  btScalarkSS_SPLT_CL;// Soft vs rigid impulse split [0,1] (cluster only)
  btScalarmaxvolume;// Maximum volume ratio for pose
  btScalartimescale;// Time scale
  intviterations;// Velocities solver iterations
  intpiterations;// Positions solver iterations
  intditerations;// Drift solver iterations
  intciterations;// Cluster solver iterations
  intcollisions;// Collisions flags
  //tVSolverArraym_vsequence;// Velocity solvers sequence
  //tPSolverArraym_psequence;// Position solvers sequence
  //tPSolverArraym_dsequence;// Drift solvers sequence
};

struct Node : Feature
{
  btVector3m_x;// Position
  btVector3m_q;// Previous step position
  btVector3m_v;// Velocity
  btVector3m_f;// Force accumulator
  btVector3m_n;// Normal
  btScalarm_im;// 1/mass
  btScalarm_area;// Area
  btDbvtNode*m_leaf;// Leaf data
 intm_battach:1;// Attached
};

%nestedworkaround btSoftBody::Element;
%nestedworkaround btSoftBody::Material;
%nestedworkaround btSoftBody::fCollision;
%nestedworkaround btSoftBody::Feature;
%nestedworkaround btSoftBody::Config;
%nestedworkaround btSoftBody::Node;

	struct	btRigidBodyConstructionInfo
	{
		btScalar			m_mass;

		///When a motionState is provided, the rigid body will initialize its world transform from the motion state
		///In this case, m_startWorldTransform is ignored.
		btMotionState*		m_motionState;
		btTransform	m_startWorldTransform;

		btCollisionShape*	m_collisionShape;
		btVector3			m_localInertia;
		btScalar			m_linearDamping;
		btScalar			m_angularDamping;

		///best simulation results when friction is non-zero
		btScalar			m_friction;
		///the m_rollingFriction prevents rounded shapes, such as spheres, cylinders and capsules from rolling forever.
		///See Bullet/Demos/RollingFrictionDemo for usage
		btScalar			m_rollingFriction;
		///best simulation results using zero restitution.
		btScalar			m_restitution;

		btScalar			m_linearSleepingThreshold;
		btScalar			m_angularSleepingThreshold;

		//Additional damping can help avoiding lowpass jitter motion, help stability for ragdolls etc.
		//Such damping is undesirable, so once the overall simulation quality of the rigid body dynamics system has improved, this should become obsolete
		bool				m_additionalDamping;
		btScalar			m_additionalDampingFactor;
		btScalar			m_additionalLinearDampingThresholdSqr;
		btScalar			m_additionalAngularDampingThresholdSqr;
		btScalar			m_additionalAngularDampingFactor;

		btRigidBodyConstructionInfo(	btScalar mass, btMotionState* motionState, btCollisionShape* collisionShape, const btVector3& localInertia=btVector3(0,0,0)):
		m_mass(mass),
			m_motionState(motionState),
			m_collisionShape(collisionShape),
			m_localInertia(localInertia),
			m_linearDamping(btScalar(0.)),
			m_angularDamping(btScalar(0.)),
			m_friction(btScalar(0.5)),
			m_rollingFriction(btScalar(0)),
			m_restitution(btScalar(0.)),
			m_linearSleepingThreshold(btScalar(0.8)),
			m_angularSleepingThreshold(btScalar(1.f)),
			m_additionalDamping(false),
			m_additionalDampingFactor(btScalar(0.005)),
			m_additionalLinearDampingThresholdSqr(btScalar(0.01)),
			m_additionalAngularDampingThresholdSqr(btScalar(0.01)),
			m_additionalAngularDampingFactor(btScalar(0.01))
		{
			m_startWorldTransform.setIdentity();
		}
	};


%nestedworkaround btRigidBody::btRigidBodyConstructionInfo;

%include carrays.i
%include stl.i
%include "btBulletDynamicsCommon.h"
%include "btBulletCollisionCommon.h"

%include "LinearMath/btScalar.h"
%include "LinearMath/btVector3.h"
%include "LinearMath/btQuadWord.h"
%include "LinearMath/btQuaternion.h"
%include "LinearMath/btTransform.h"
%include "LinearMath/btMatrix3x3.h"
%include "LinearMath/btMotionState.h"
%include "LinearMath/btAlignedObjectArray.h"
%include "LinearMath/btDefaultMotionState.h"

%include "BulletCollision/BroadphaseCollision/btBroadphaseProxy.h"
%include "BulletCollision/BroadphaseCollision/btBroadphaseInterface.h"
%include "BulletCollision/BroadphaseCollision/btDbvtBroadphase.h"
%include "BulletCollision/BroadphaseCollision/btDispatcher.h"

%include "BulletCollision/CollisionDispatch/btCollisionConfiguration.h"
%include "BulletCollision/CollisionDispatch/btDefaultCollisionConfiguration.h"
%include "BulletCollision/CollisionDispatch/btCollisionDispatcher.h"
%include "BulletCollision/CollisionDispatch/btCollisionObject.h"
%include "BulletCollision/CollisionDispatch/btCollisionWorld.h"

%include "BulletCollision/CollisionShapes/btCollisionShape.h"
%include "BulletCollision/CollisionShapes/btConvexShape.h"
%include "BulletCollision/CollisionShapes/btConvexInternalShape.h"
%include "BulletCollision/CollisionShapes/btPolyhedralConvexShape.h"
%include "BulletCollision/CollisionShapes/btConvexHullShape.h"
%include "BulletCollision/CollisionShapes/btSphereShape.h"
%include "BulletCollision/CollisionShapes/btMultiSphereShape.h"
%include "BulletCollision/CollisionShapes/btConcaveShape.h"
%include "BulletCollision/CollisionShapes/btStaticPlaneShape.h"
%include "BulletCollision/CollisionShapes/btBoxShape.h"
%include "BulletCollision/CollisionShapes/btCylinderShape.h"
%include "BulletCollision/CollisionShapes/btCapsuleShape.h"

%include "BulletDynamics/Dynamics/btDynamicsWorld.h"
%include "BulletDynamics/Dynamics/btDiscreteDynamicsWorld.h"
%include "BulletDynamics/Dynamics/btSimpleDynamicsWorld.h"
%include "BulletDynamics/Dynamics/btRigidBody.h"

%include "BulletDynamics/ConstraintSolver/btConstraintSolver.h"
%include "BulletDynamics/ConstraintSolver/btTypedConstraint.h"
%include "BulletDynamics/ConstraintSolver/btPoint2PointConstraint.h"
%include "BulletDynamics/ConstraintSolver/btHingeConstraint.h"
%include "BulletDynamics/ConstraintSolver/btConeTwistConstraint.h"
%include "BulletDynamics/ConstraintSolver/btGeneric6DofConstraint.h"
%include "BulletDynamics/ConstraintSolver/btSliderConstraint.h"
%include "BulletDynamics/ConstraintSolver/btGeneric6DofSpringConstraint.h"
%include "BulletDynamics/ConstraintSolver/btUniversalConstraint.h"
%include "BulletDynamics/ConstraintSolver/btHinge2Constraint.h"
%include "BulletDynamics/ConstraintSolver/btSequentialImpulseConstraintSolver.h"

%ignore btSoftBody::m_ndbvt;
%ignore btSoftBody::m_cdbvt;
%ignore btSoftBody::m_fdbvt;


%include "BulletSoftBody/btSparseSDF.h"
%include "BulletSoftBody/btSoftBody.h"
%include "BulletSoftBody/btSoftRigidDynamicsWorld.h"
%include "BulletSoftBody/btSoftBodySolvers.h"
%include "BulletSoftBody/btDefaultSoftBodySolver.h"
%include "BulletSoftBody/btSoftBodyHelpers.h"
%include "BulletSoftBody/btSoftBodyRigidBodyCollisionConfiguration.h"

%{

EXPORT btDbvt *_wrap_btSoftBody_m_ndbvt_get (btSoftBody *larg1) {
    btDbvt * lresult = (btDbvt *)0 ;
    btSoftBody *arg1 = (btSoftBody *) 0 ;
  
    arg1 = larg1;
    try {
      return &((arg1)->m_ndbvt);
    } catch (...) {
      return (btDbvt *)0;
    }
  }
 


 EXPORT btDbvt *_wrap_btSoftBody_m_fdbvt_get (btSoftBody *larg1) {
   btDbvt * lresult = (btDbvt *)0 ;
   btSoftBody *arg1 = (btSoftBody *) 0 ;
   
   arg1 = larg1;
   try {
     return &((arg1)->m_fdbvt);
   } catch (...) {
     return (btDbvt *)0;
   }
 }
  
EXPORT btDbvt *_wrap_btSoftBody_m_cdbvt_get (btSoftBody *larg1) {
  btDbvt * lresult = (btDbvt *)0 ;
  btSoftBody *arg1 = (btSoftBody *) 0 ;
  
  arg1 = larg1;
  try {
    return &((arg1)->m_cdbvt);
  } catch (...) {
    return (btDbvt *)0;
  }
}


  %}


%inline %{

  const btVector3& btSoftBodyGetNodePosition(const btSoftBody *body, int n) {
    return body->m_nodes[n].m_x;
  }

  void btSoftBodySetNodePosition(btSoftBody *body, int n, const btVector3& pos) {
    body->m_nodes[n].m_x = pos;
  }

  void btSoftBodySetStretch(btSoftBody *body, float val) {
    body->m_materials[0]->m_kLST = val;
  }

  void btSoftBodySetShear(btSoftBody *body, float val) {
    body->m_materials[0]->m_kAST = val;
  }
  %}


%template(btSparseSdf3) btSparseSdf<3>;
%array_class(float, floatArray);
%array_class(btVector3, btVector3Array);

