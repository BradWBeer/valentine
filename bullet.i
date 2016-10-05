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


%nestedworkaround btSoftBody::Element;
%nestedworkaround btSoftBody::Material;
%nestedworkaround btSoftBody::fCollision;
%nestedworkaround btSoftBody::Feature;
%nestedworkaround btSoftBody::Config;
%nestedworkaround btSoftBody::Node;
%nestedworkaround btRigidBody::btRigidBodyConstructionInfo;

%ignore btSoftBody::m_ndbvt;
%ignore btSoftBody::m_cdbvt;
%ignore btSoftBody::m_fdbvt;

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

%include "BulletSoftBody/btSparseSDF.h"
%include "BulletSoftBody/btSoftBody.h"
%include "BulletSoftBody/btSoftRigidDynamicsWorld.h"
%include "BulletSoftBody/btSoftBodySolvers.h"
%include "BulletSoftBody/btDefaultSoftBodySolver.h"
%include "BulletSoftBody/btSoftBodyHelpers.h"
%include "BulletSoftBody/btSoftBodyRigidBodyCollisionConfiguration.h"




