const fakeBodyCount = 10
const fakeBodySteps = 500

// Decorate the head of our guests
Vue.component("obj-head", {
	template: `<a-entity>
		<a-sphere
			shadow
			:radius="headSize"
			:color="obj.color.toHex()"

			>
			<obj-axes scale=".1 .1 .1" v-if="true" />
		</a-sphere>

		<a-entity v-if="hat"
			position="0 0.2 0"
			>
			<a-cylinder
				height="0.025"
				:radius="hat.r"
				:color="hat.color.toHex(0,0.5)"
				>
			</a-cylinder>
			<a-cylinder
				:height="hat.h"
				:radius="hat.r - 0.1"
				:color="hat.color.toHex(0,0.5)"
				:position="hat.pos.toAFrame()"
				>
			</a-cylinder>

		</a-entity>
	</a-entity>
	`,
	computed: {
		color() {
			return this.obj.color.toHex?this.obj.color.toHex():this.obj.color
		},
		headSize() {
			return this.obj.size instanceof Vector ? this.obj.size.x : this.obj.size
		},
	},

	data() {
		let hat = new LiveObject(undefined, {
			r: 0.1 + Math.abs(noise(this.obj.room.time.t)*0.5),
			h: Math.abs(noise(this.obj.room.time.t) + 0.5),
			color: new Vector(noise(this.obj.room.time.t)*360, 100, 40 + 20*noise(this.obj.room.time.t*3))
		})
		hat.pos = new Vector(0, hat.h/2, 0)
		console.log(hat.color.toCSSColor(0,0.5))
		//let r = .2
		// Put them on the other side
		// let theta = 2*noise(this.room.time.t*10) + 3
		// spike.position.setToCylindrical(r, theta, h*.3)
		// // Look randomly
		// spike.lookAt(0, 3, 0)

		return {
			hat: hat
		}
	},

	mounted() {
		console.log(this.headSize)
	},
	props: ["obj"]
})


Vue.component("obj-fire", {
	template: `
	<a-entity>
		<a-sphere
			color="grey"
			radius=2
			scale="1 .3 1"
			roughness=1
			segments-height="5"
			segments-width="10"
			theta-start=0
			theta-length=60
			position="0 -.4 0"
			>
		</a-sphere>
		<a-cone
			position="0 .2 0"
			@click="click"
			:animation="heightAnimation"
			:color="obj.color.toHex()"
			height=.2
			radius-bottom=".2"

			:scale="(obj.fireStrength*.2 + 1) + ' ' + .1*obj.fireStrength + ' ' + (obj.fireStrength*.2 + 1)"
			:material="fireMaterial">

		</a-cone>

		<a-light
			:animation="intensityAnimation"

			position="0 1 0"
			intensity="2"
			:color="obj.color.toHex()"
			type="point"
			:distance="obj.fireStrength*4 + 10"
			decay="2">
		</a-light>
	</a-entity>

	`,

	// Values computed on the fly
	computed: {
		fireMaterial() {
			return `emissive:${this.obj.color.toHex(.2)}`
		},

		animationSpeed() {
			return 500
		},
		intensityAnimation() {
			return `property: intensity; from:.3; to:.6; dir:alternate;dur: ${this.animationSpeed}; easing:easeInOutQuad;loop:true`
		},
		heightAnimation() {
			return `property: height; from:${this.obj.fireStrength};to:${this.obj.fireStrength*2}; dir:alternate;dur: 500; easing:easeInOutQuad;loop:true`
		}
	},

	methods: {
		click() {
			this.obj.fireStrength += 1
			this.obj.fireStrength = this.obj.fireStrength%10 + 1

			Vue.set(this.obj.color.v, 0, Math.random()*360)

			// Tell the server about this action
			this.obj.post()
		}
	},

	// this function runs once when this object is created
	mounted() {

	},



	props: ["obj"]


})



Vue.component("obj-world", {

	template: `
	<a-entity>
		<!--------- SKYBOX --------->
		<a-sky color="lightblue"></a-sky>

		<a-plane
			roughness="1"
			shadow
			color="hsl(184,40%,55%)"
			height="100"
			width="100"
			rotation="-90 0 0">
		</a-plane>

		<!---- lights ---->
		<a-entity light="type: ambient; intensity: 0.4;" color="white"></a-entity>
		<a-light type="directional"
			position="0 0 0"
			rotation="-90 0 0"
			intensity="0.4"
			castShadow target="#directionaltarget">
			<a-entity id="directionaltarget" position="-10 0 -20"></a-entity>
		</a-light>

		<!-- <a-cone
			v-for="(tree,index) in trees"
			:key="'tree' + index"
			shadow

			:color="tree.color.toHex()"
			:base-radius="tree.size.z"
			:height="tree.size.y"

			segments-radial=10
			segments-height=1

			:rotation="tree.rotation.toAFrame()"
			:position="tree.position.toAFrame()">
		</a-cone> -->

		<!-- snowmen -->
		<a-entity>
			<a-sphere
				v-for="(snowman, index) in snowmen"
				:key="'snowmanBase' + index"
				:position="snowman.position.x.toAFrame()"
				:radius="snowman.size.x"
				color="#FFFFFF">
			</a-sphere>
			<a-sphere
				v-for="(snowman, index) in snowmen"
				:key="'snowmanMiddle' + index"
				:position="snowman.position.y.toAFrame()"
				:radius="snowman.size.y"
				color="#FFFFFF">
			</a-sphere>
			<a-sphere
				v-for="(snowman, index) in snowmen"
				:key="'snowmanTop' + index"
				:position="snowman.position.z.toAFrame()"
				:radius="snowman.size.z"
				color="#FFFFFF">
			</a-sphere>

			<a-sphere
				v-for="(snowman, index) in snowmen"
				:key="'snowmanLeftEye' + index"
				:position="snowman.leftEyePosition"
				:radius=".05"
				color="#000000">
			</a-sphere>
			<a-sphere
				v-for="(snowman, index) in snowmen"
				:key="'snowmanRightEye' + index"
				:position="snowman.rightEyePosition"
				:radius=".05"
				color="#000000">
			</a-sphere>

			<a-sphere
				v-for="(snowman, index) in snowmen"
				:key="'snowmanNose' + index"
				:position="snowman.nosePosition"
				:radius=".05"
				scale="5 1"
				color="#FFA500">
			</a-sphere>

		</a-entity>


		<!-- igloos -->
		<a-entity>
			<a-sphere
				v-for="(igloo, index) in igloos"
				:key="'iglooBase' + index"
				:position="igloo.position.x.toAFrame()"
				:radius="igloo.size.x"
				color="#FFFFFF">
			</a-sphere>

			<a-box scale="1 5 1"
				v-for="(igloo, index) in igloos"
				:key="'iglooChimney' + index"
				:position="igloo.chimneyPosition"
				:radius=".5"
				color="#54585B">
			</a-box>

			<a-box scale="5 5 3"
				v-for="(igloo, index) in igloos"
				:key="'iglooDoor' + index"
				:position="igloo.doorPosition"
				:radius=".5"
				src="#/img/textures/igloos-wall.png">
			</a-box>




		</a-entity>



		<!-- <a-box
			v-for="(rock,index) in rocks"
			:key="'rock' + index"
			shadow

			roughness="1"

			:color="rock.color.toHex()"
			:width="rock.size.x"
			:depth="rock.size.z"
			:height="rock.size.y"

			:rotation="rock.rotation.toAFrame()"
			:position="rock.position.toAFrame()">
		</a-box> -->

	</a-entity>
		`,

	data() {
		// Where we setup the data that this *rendered scene needs*

		// EXAMPLE: Generated landscape
		// Make some random trees and rocks
		// Create a lot of LiveObjects (just as a way
		//  to store size and color conveniently)
		// Interpret them as whatever A-Frame geometry you want!
		// Cones, spheres, entities with multiple ...things?
		// If you only use "noise" and not "random",
		// everyone will have the same view. (Wordle-style!)
		let trees = []
		let count = 30
		for (var i = 0; i < count; i++) {
			let h = 6 + 4*noise(i) // Size from 1 to 3
			let tree = new LiveObject(undefined, {
				size: new THREE.Vector3(.3, h, .3),
				color: new Vector(noise(i*50)*30 + 160, 100, 40 + 10*noise(i*10))
			})
			let r = 20 + 10*noise(i*40)
			let theta = 2*noise(i*10)
			tree.position.setToCylindrical(r, theta, h/2)
			tree.lookAt(0,1,0)
			trees.push(tree)
		}

		let rocks = []
		let rockCount = 20
		for (var i = 0; i < rockCount; i++) {
			let h = 1.2 + noise(i*100) // Size from 1 to 3
			let rock = new LiveObject(undefined, {
				size: new THREE.Vector3(h, h, h),
				color: new Vector(noise(i)*30 + 140, 0, 40 + 20*noise(i*3))
			})
			let r = 4 + 1*noise(i*1)
			// Put them on the other side
			let theta = 2*noise(i*10) + 3
			rock.position.setToCylindrical(r, theta, h*.3)
			// Look randomly
			rock.lookAt(Math.random()*100,Math.random()*100,Math.random()*100)
			rocks.push(rock)
		}
		console.log("rocks[0]: ", rocks[0])

		let snowmenCount = 20;
		let snowmen = createSnowmen(snowmenCount)

		let igloosCount = 4;
		let igloos = createIgloos(igloosCount)

		return {
			trees: trees,
			rocks: rocks,
			snowmen: snowmen,
			igloos: igloos
		}
	},

	mounted() {
		// Create a fire object
		// Attach this liveobject to the ROOM
		// and then the room deals with drawing it to AFRAME
		let fire = new LiveObject(this.room, {
			paritype: "fire",  // Tells it which type to use
			uid: "fire0",
			// onUpdate({t, dt, frameCount}) {
			// 	let hue = (noise(t*.02)+1)*180
			// 	Vue.set(this.color.v, 0, hue)

			// 	// console.log(hue)
			// }
		})

		fire.position.set(1, 0, -2)
		fire.fireStrength = 1

		// let fire2 = new LiveObject(this.room, {
		// 	paritype: "fire",  // Tells it which type to use
		// 	uid: "fire2",
		// 	onUpdate({t, dt, frameCount}) {
		// 		let hue = (noise(t*.02)+1)*180
		// 		Vue.set(this.color.v, 0, hue)

		// 		// console.log(this.color[0] )
		// 	}
		// })

		// fire2.position.set(3, 0, -4)
		// fire2.fireStrength = 7

		this.room.detailText = "You've been assigned a new word! Talk to others in the room to see if you can guess the word above your head."

		this.room.time.onSecondChange((second) => {
			// Change the song every minute (60 seconds)
			let rate = 60 // How many seconds between changes
			if (second%10 === 0) {
				this.room.detailText = ""
			}
			if (second%rate === 0) {
				for (const [uid, obj] of Object.entries(this.room.objects)) {
					if (obj.paritype === "head") {
						Vue.set(obj, 'label', grammar.flatten("#word#"))
					}
				}
				this.room.detailText = "You've been assigned a new word! Talk to others in the room to see if you can guess the word above your head."
			}
		})
	},

	props: ["room"]

})
