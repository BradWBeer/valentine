(ql:quickload :valentine)

;; collision configuration contains default setup for memory, collision setup. Advanced users can create their own configuration.

(setf collision-Configuration (valentine::new_btDefaultCollisionConfiguration))

;; use the default collision dispatcher. For parallel processing you can use a diffent dispatcher (see Extras/BulletMultiThreaded)
 (setf dispacher (valentine::new_btcollisiondispatcher collision-configuration ))


;; btDbvtBroadphase is a good general purpose broadphase. You can also try out btAxis3Sweep.
(setf overlapping-pair-cache (cffi:foreign-alloc 'valentine::btdbvtbroadphase))

;; the default constraint solver. For parallel processing you can use a different solver (see Extras/BulletMultiThreaded)
(setf solver (valentine::new_btsequentialimpulseconstraintsolver))

(setf dynamics-world (valentine::new_btdiscretedynamicsworld dispacher overlapping-pair-cache solver collision-configuration))

(valentine::btdiscretedynamicsworld_setgravity
 dynamics-world (valentine::make-btVector3 0 10 0))

;; -----initialization_end-----

;; create a few basic rigid bodies
(setf groundShape (valentine::new_btboxshape (valentine::make-btvector3 50 50 50)))

;; keep track of the shapes, we release memory at exit.
;; make sure to re-use collision shapes among rigid bodies whenever possible!
(setf collision-shapes nil)
(push groundshape collision-shapes)

(setf ground-transform (valentine::new_btTransform))
(valentine::bttransform_setidentity ground-transform)
(valentine::bttransform_setorigin ground-transform (valentine::make-btvector3 0 -56 0))

(cffi:with-foreign-object (mass :float)
  (setf (cffi:mem-aref mass :float) (coerce 0 'single-float))

;; using motionstate is optional, it provides interpolation capabilities, and only synchronizes 'active' objects
;; 		btDefaultMotionState* myMotionState = new btDefaultMotionState(groundTransform);
;; 		btRigidBody::btRigidBodyConstructionInfo rbInfo(mass,myMotionState,groundShape,localInertia);
;; 		btRigidBody* body = new btRigidBody(rbInfo);

;; 		//add the body to the dynamics world
;; 		dynamicsWorld->addRigidBody(body);
;; 	}


;; 	{
;; 		//create a dynamic rigidbody

;; 		//btCollisionShape* colShape = new btBoxShape(btVector3(1,1,1));
;; 		btCollisionShape* colShape = new btSphereShape(btScalar(1.));
;; 		collisionShapes.push_back(colShape);

;; 		/// Create Dynamic Objects
;; 		btTransform startTransform;
;; 		startTransform.setIdentity();

;; 		btScalar	mass(1.f);

;; 		//rigidbody is dynamic if and only if mass is non zero, otherwise static
;; 		bool isDynamic = (mass != 0.f);

;; 		btVector3 localInertia(0,0,0);
;; 		if (isDynamic)
;; 			colShape->calculateLocalInertia(mass,localInertia);

;; 			startTransform.setOrigin(btVector3(2,10,0));
		
;; 			//using motionstate is recommended, it provides interpolation capabilities, and only synchronizes 'active' objects
;; 			btDefaultMotionState* myMotionState = new btDefaultMotionState(startTransform);
;; 			btRigidBody::btRigidBodyConstructionInfo rbInfo(mass,myMotionState,colShape,localInertia);
;; 			btRigidBody* body = new btRigidBody(rbInfo);

;; 			dynamicsWorld->addRigidBody(body);
;; 	}



;; /// Do some simulation


;; 	///-----stepsimulation_start-----
;; 	for (i=0;i<100;i++)
;; 	{
;; 		dynamicsWorld->stepSimulation(1.f/60.f,10);
		
;; 		//print positions of all objects
;; 		for (int j=dynamicsWorld->getNumCollisionObjects()-1; j>=0 ;j--)
;; 		{
;; 			btCollisionObject* obj = dynamicsWorld->getCollisionObjectArray()[j];
;; 			btRigidBody* body = btRigidBody::upcast(obj);
;; 			btTransform trans;
;; 			if (body && body->getMotionState())
;; 			{
;; 				body->getMotionState()->getWorldTransform(trans);

;; 			} else
;; 			{
;; 				trans = obj->getWorldTransform();
;; 			}
;; 			printf("world pos object %d = %f,%f,%f\n",j,float(trans.getOrigin().getX()),float(trans.getOrigin().getY()),float(trans.getOrigin().getZ()));
;; 		}
;; 	}

;; 	///-----stepsimulation_end-----

;; 	//cleanup in the reverse order of creation/initialization
	
;; 	///-----cleanup_start-----

;; 	//remove the rigidbodies from the dynamics world and delete them
;; 	for (i=dynamicsWorld->getNumCollisionObjects()-1; i>=0 ;i--)
;; 	{
;; 		btCollisionObject* obj = dynamicsWorld->getCollisionObjectArray()[i];
;; 		btRigidBody* body = btRigidBody::upcast(obj);
;; 		if (body && body->getMotionState())
;; 		{
;; 			delete body->getMotionState();
;; 		}
;; 		dynamicsWorld->removeCollisionObject( obj );
;; 		delete obj;
;; 	}

;; 	//delete collision shapes
;; 	for (int j=0;j<collisionShapes.size();j++)
;; 	{
;; 		btCollisionShape* shape = collisionShapes[j];
;; 		collisionShapes[j] = 0;
;; 		delete shape;
;; 	}

;; 	//delete dynamics world
;; 	delete dynamicsWorld;

;; 	//delete solver
;; 	delete solver;

;; 	//delete broadphase
;; 	delete overlappingPairCache;

;; 	//delete dispatcher
;; 	delete dispatcher;

;; 	delete collisionConfiguration;

;; 	//next line is optional: it will be cleared by the destructor when the array goes out of scope
;; 	collisionShapes.clear();

;; 	///-----cleanup_end-----
;; 	printf("Press a key to exit\n");
;; 	getchar();
;; }

