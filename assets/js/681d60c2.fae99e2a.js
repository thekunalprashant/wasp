"use strict";(self.webpackChunkweb=self.webpackChunkweb||[]).push([[5850],{3905:(e,t,n)=>{n.d(t,{Zo:()=>u,kt:()=>g});var a=n(67294);function i(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function r(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?r(Object(n),!0).forEach((function(t){i(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):r(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,a,i=function(e,t){if(null==e)return{};var n,a,i={},r=Object.keys(e);for(a=0;a<r.length;a++)n=r[a],t.indexOf(n)>=0||(i[n]=e[n]);return i}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(a=0;a<r.length;a++)n=r[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(i[n]=e[n])}return i}var p=a.createContext({}),l=function(e){var t=a.useContext(p),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},u=function(e){var t=l(e.components);return a.createElement(p.Provider,{value:t},e.children)},d="mdxType",c={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},m=a.forwardRef((function(e,t){var n=e.components,i=e.mdxType,r=e.originalType,p=e.parentName,u=s(e,["components","mdxType","originalType","parentName"]),d=l(n),m=i,g=d["".concat(p,".").concat(m)]||d[m]||c[m]||r;return n?a.createElement(g,o(o({ref:t},u),{},{components:n})):a.createElement(g,o({ref:t},u))}));function g(e,t){var n=arguments,i=t&&t.mdxType;if("string"==typeof e||i){var r=n.length,o=new Array(r);o[0]=m;var s={};for(var p in t)hasOwnProperty.call(t,p)&&(s[p]=t[p]);s.originalType=e,s[d]="string"==typeof e?e:i,o[1]=s;for(var l=2;l<r;l++)o[l]=n[l];return a.createElement.apply(null,o)}return a.createElement.apply(null,n)}m.displayName="MDXCreateElement"},38610:(e,t,n)=>{n.d(t,{Z:()=>r});var a=n(67294),i=n(44996);const r=e=>a.createElement("div",null,a.createElement("p",{align:"center"},a.createElement("figure",null,a.createElement("img",{style:{width:e.width},alt:e.alt,src:(0,i.Z)(e.source)}),a.createElement("figcaption",{class:"image-caption",style:{fontStyle:"italic",opacity:.6,fontSize:"0.9rem"}},e.caption))))},46393:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>l,contentTitle:()=>s,default:()=>m,frontMatter:()=>o,metadata:()=>p,toc:()=>u});var a=n(87462),i=(n(67294),n(3905)),r=n(38610);const o={title:"Automatic CRUD"},s=void 0,p={unversionedId:"guides/crud",id:"guides/crud",title:"Automatic CRUD",description:"For some Entity, you can tell Wasp to automatically generate server-side logic (Queries and Actions) for creating, reading, updating and deleting such entities. As your entities update, Wasp will automatically regenerate the backend logic.",source:"@site/docs/guides/crud.md",sourceDirName:"guides",slug:"/guides/crud",permalink:"/docs/guides/crud",draft:!1,editUrl:"https://github.com/wasp-lang/wasp/edit/main/web/docs/guides/crud.md",tags:[],version:"current",frontMatter:{title:"Automatic CRUD"},sidebar:"docs",previous:{title:"Middleware Customization",permalink:"/docs/guides/middleware-customization"},next:{title:"WebSockets",permalink:"/docs/guides/websockets"}},l={},u=[{value:"Defining new CRUD operations",id:"defining-new-crud-operations",level:2},{value:"Example: simple TODO app",id:"example-simple-todo-app",level:2},{value:"Creating the app",id:"creating-the-app",level:3},{value:"Adding CRUD to the <code>Task</code> entity \u2728",id:"adding-crud-to-the-task-entity-",level:3},{value:"Our custom <code>create</code> operation",id:"our-custom-create-operation",level:3},{value:"Using the generated CRUD operations on the client",id:"using-the-generated-crud-operations-on-the-client",level:3},{value:"Future of CRUD operations in Wasp",id:"future-of-crud-operations-in-wasp",level:3}],d={toc:u},c="wrapper";function m(e){let{components:t,...n}=e;return(0,i.kt)(c,(0,a.Z)({},d,n,{components:t,mdxType:"MDXLayout"}),(0,i.kt)("p",null,"For some ",(0,i.kt)("a",{parentName:"p",href:"/docs/language/features#entity"},"Entity"),", you can tell Wasp to automatically generate server-side logic (",(0,i.kt)("a",{parentName:"p",href:"/docs/language/features#query"},"Queries")," and ",(0,i.kt)("a",{parentName:"p",href:"/docs/language/features#action"},"Actions"),") for creating, reading, updating and deleting such entities. As your entities update, Wasp will automatically regenerate the backend logic."),(0,i.kt)("admonition",{title:"Early preview",type:"caution"},(0,i.kt)("p",{parentName:"admonition"},"This feature is currently in early preview and we are actively working on it. Read more about ",(0,i.kt)("a",{parentName:"p",href:"/docs/language/features#crud-operations-on-top-of-entities"},"our plans")," for CRUD operations.")),(0,i.kt)("h2",{id:"defining-new-crud-operations"},"Defining new CRUD operations"),(0,i.kt)("p",null,"Imagine we have a ",(0,i.kt)("inlineCode",{parentName:"p"},"Task")," entity and we want to enable CRUD operations for it."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-wasp",metastring:'title="main.wasp"',title:'"main.wasp"'},"entity Task {=psl\n  id          Int @id @default(autoincrement())\n  description String\n  isDone      Boolean\npsl=}\n")),(0,i.kt)("p",null,"We can then define a new ",(0,i.kt)("inlineCode",{parentName:"p"},"crud")," called ",(0,i.kt)("inlineCode",{parentName:"p"},"Tasks"),"."),(0,i.kt)("p",null,"We specify to use the ",(0,i.kt)("inlineCode",{parentName:"p"},"Task")," entity and we enable the ",(0,i.kt)("inlineCode",{parentName:"p"},"getAll"),", ",(0,i.kt)("inlineCode",{parentName:"p"},"get"),", ",(0,i.kt)("inlineCode",{parentName:"p"},"create")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"update")," operations (let's say we don't need the ",(0,i.kt)("inlineCode",{parentName:"p"},"delete")," operation)."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-wasp",metastring:'title="main.wasp"',title:'"main.wasp"'},'crud Tasks {\n  entity: Task,\n  operations: {\n    getAll: {\n      isPublic: true, // by default only logged in users can perform operations\n    },\n    get: {},\n    create: {\n      overrideFn: import { createTask } from "@server/tasks.js",\n    },\n    update: {},\n  },\n}\n')),(0,i.kt)("ol",null,(0,i.kt)("li",{parentName:"ol"},"It uses default implementation for ",(0,i.kt)("inlineCode",{parentName:"li"},"getAll"),", ",(0,i.kt)("inlineCode",{parentName:"li"},"get")," and ",(0,i.kt)("inlineCode",{parentName:"li"},"update"),","),(0,i.kt)("li",{parentName:"ol"},"... while specifying a custom implementation for ",(0,i.kt)("inlineCode",{parentName:"li"},"create"),". "),(0,i.kt)("li",{parentName:"ol"},(0,i.kt)("inlineCode",{parentName:"li"},"getAll")," will be public (no auth needed), while the rest of the operations will be private.")),(0,i.kt)("p",null,"Here's what it looks like when visualized:"),(0,i.kt)(r.Z,{alt:"Automatic CRUD with Wasp",source:"img/crud_diagram.png",caption:"Visualization of the Tasks crud declaration",mdxType:"ImgWithCaption"}),(0,i.kt)("p",null,"We can now use the CRUD queries and actions we just specified in our client code."),(0,i.kt)("h2",{id:"example-simple-todo-app"},"Example: simple TODO app"),(0,i.kt)("p",null,"Let's create a full app example that uses automatic CRUD. We'll stick to using the ",(0,i.kt)("inlineCode",{parentName:"p"},"Task")," entity from the previous example, but we'll add a ",(0,i.kt)("inlineCode",{parentName:"p"},"User")," entity and enable ",(0,i.kt)("a",{parentName:"p",href:"/docs/language/features#username-and-password"},"username and password")," based auth."),(0,i.kt)(r.Z,{alt:"Automatic CRUD with Wasp",source:"img/crud-guide.gif",caption:"We are building a simple tasks app with username based auth",mdxType:"ImgWithCaption"}),(0,i.kt)("h3",{id:"creating-the-app"},"Creating the app"),(0,i.kt)("p",null,"We can start by running ",(0,i.kt)("inlineCode",{parentName:"p"},"wasp new tasksCrudApp")," and then we'll add the following to our ",(0,i.kt)("inlineCode",{parentName:"p"},"main.wasp")," file:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-wasp",metastring:'title="main.wasp"',title:'"main.wasp"'},'app tasksCrudApp {\n  wasp: {\n    version: "^0.11.0"\n  },\n  title: "Tasks Crud App",\n\n  // We enabled auth and set the auth method to username and password\n  auth: {\n    userEntity: User,\n    methods: {\n      usernameAndPassword: {},\n    },\n    onAuthFailedRedirectTo: "/login",\n  },\n}\n\nentity User {=psl\n  id       Int @id @default(autoincrement())\n  username String @unique\n  password String\n  tasks    Task[]\npsl=}\n\n// We defined a Task entity on which we\'ll enable CRUD later on\nentity Task {=psl\n  id          Int @id @default(autoincrement())\n  description String\n  isDone      Boolean\n  userId      Int\n  user        User @relation(fields: [userId], references: [id])\npsl=}\n\n// Tasks app routes\nroute RootRoute { path: "/", to: MainPage }\npage MainPage {\n  component: import { MainPage } from "@client/MainPage.jsx",\n  authRequired: true,\n}\n\nroute LoginRoute { path: "/login", to: LoginPage }\npage LoginPage {\n  component: import { LoginPage } from "@client/LoginPage.jsx",\n}\n\nroute SignupRoute { path: "/signup", to: SignupPage }\npage SignupPage {\n  component: import { SignupPage } from "@client/SignupPage.jsx",\n}\n')),(0,i.kt)("p",null,"We can then run ",(0,i.kt)("inlineCode",{parentName:"p"},"wasp db migrate-dev")," to create the database and run the migrations."),(0,i.kt)("h3",{id:"adding-crud-to-the-task-entity-"},"Adding CRUD to the ",(0,i.kt)("inlineCode",{parentName:"h3"},"Task")," entity \u2728"),(0,i.kt)("p",null,"Let's add the following ",(0,i.kt)("inlineCode",{parentName:"p"},"crud")," declaration to our ",(0,i.kt)("inlineCode",{parentName:"p"},"main.wasp")," file:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-wasp",metastring:'title="main.wasp"',title:'"main.wasp"'},'// ...\n\ncrud Tasks {\n  entity: Task,\n  operations: {\n    getAll: {},\n    create: {\n      overrideFn: import { createTask } from "@server/tasks.js",\n    },\n  },\n}\n')),(0,i.kt)("p",null,"You'll notice that we enabled only ",(0,i.kt)("inlineCode",{parentName:"p"},"getAll")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"create")," operations. This means that only these operations will be available."),(0,i.kt)("p",null,"We also overrode the ",(0,i.kt)("inlineCode",{parentName:"p"},"create")," operation with a custom implementation. This means that the ",(0,i.kt)("inlineCode",{parentName:"p"},"create")," operation will not be generated, but instead, the ",(0,i.kt)("inlineCode",{parentName:"p"},"createTask")," function from ",(0,i.kt)("inlineCode",{parentName:"p"},"@server/tasks.js")," will be used."),(0,i.kt)("h3",{id:"our-custom-create-operation"},"Our custom ",(0,i.kt)("inlineCode",{parentName:"h3"},"create")," operation"),(0,i.kt)("p",null,"Here's the ",(0,i.kt)("inlineCode",{parentName:"p"},"src/server/tasks.js")," file:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-ts",metastring:'title="src/server/tasks.ts" {20-25}',title:'"src/server/tasks.ts"',"{20-25}":!0},"import type { CreateAction } from '@wasp/crud/Tasks'\nimport type { Task } from '@wasp/entities'\nimport HttpError from '@wasp/core/HttpError.js'\n\ntype Input = { description: string; isDone: boolean }\ntype Output = Task\n\nexport const createTask: CreateAction<Input, Output> = async (args, context) => {\n  if (!context.user) {\n    throw new HttpError(401, 'User not authenticated.')\n  }\n\n  const { description, isDone } = args\n  const { Task } = context.entities\n\n  return await Task.create({\n    data: {\n      description,\n      isDone,\n      // Connect the task to the user that is creating it\n      user: {\n        connect: {\n          id: context.user.id,\n        },\n      },\n    },\n  })\n}\n")),(0,i.kt)("p",null,"We made a custom ",(0,i.kt)("inlineCode",{parentName:"p"},"create")," operation because we want to make sure that the task is connected to the user that is creating it. In the current iteration of CRUD operations that is not supported by default. Read more about the ",(0,i.kt)("a",{parentName:"p",href:"/docs/language/features#which-operations-are-supported"},"default implementations"),"."),(0,i.kt)("h3",{id:"using-the-generated-crud-operations-on-the-client"},"Using the generated CRUD operations on the client"),(0,i.kt)("p",null,"And let's use the generated operations in our client code:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-jsx",metastring:'title="pages/MainPage.jsx" {1,5-6}',title:'"pages/MainPage.jsx"',"{1,5-6}":!0},"import { Tasks } from '@wasp/crud/Tasks'\nimport { useState } from 'react'\n\nexport const MainPage = () => {\n  const { data: tasks, isLoading, error } = Tasks.getAll.useQuery()\n  const createTask = Tasks.create.useAction()\n  const [taskDescription, setTaskDescription] = useState('')\n\n  function handleCreateTask() {\n    createTask({ description: taskDescription, isDone: false })\n    setTaskDescription('')\n  }\n\n  if (isLoading) return <div>Loading...</div>\n  if (error) return <div>Error: {error.message}</div>\n  return (\n    <div\n      style={{\n        fontSize: '1.5rem',\n        display: 'grid',\n        placeContent: 'center',\n        height: '100vh',\n      }}\n    >\n      <div>\n        <input\n          value={taskDescription}\n          onChange={(e) => setTaskDescription(e.target.value)}\n        />\n        <button onClick={handleCreateTask}>Create task</button>\n      </div>\n      <ul>\n        {tasks.map((task) => (\n          <li key={task.id}>{task.description}</li>\n        ))}\n      </ul>\n    </div>\n  )\n}\n")),(0,i.kt)("p",null,"And here are the login and signup pages, where we are using Wasp's ",(0,i.kt)("a",{parentName:"p",href:"/docs/guides/auth-ui"},"Auth UI")," components:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-jsx",metastring:'title="src/client/LoginPage.jsx"',title:'"src/client/LoginPage.jsx"'},"import { LoginForm } from '@wasp/auth/forms/Login'\nimport { Link } from 'react-router-dom'\n\nexport function LoginPage() {\n  return (\n    <div\n      style={{\n        display: 'grid',\n        placeContent: 'center',\n      }}\n    >\n      <LoginForm />\n      <div>\n        <Link to=\"/signup\">Create an account</Link>\n      </div>\n    </div>\n  )\n}\n")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-jsx",metastring:'title="src/client/SignupPage.jsx"',title:'"src/client/SignupPage.jsx"'},"import { SignupForm } from '@wasp/auth/forms/Signup'\n\nexport function SignupPage() {\n  return (\n    <div\n      style={{\n        display: 'grid',\n        placeContent: 'center',\n      }}\n    >\n      <SignupForm />\n    </div>\n  )\n}\n")),(0,i.kt)("p",null,"That's it. You can now run ",(0,i.kt)("inlineCode",{parentName:"p"},"wasp start")," and see the app in action \u26a1\ufe0f"),(0,i.kt)("p",null,"You should see a login page and a signup page. After you log in, you should see a page with a list of tasks and a form to create new tasks."),(0,i.kt)("h3",{id:"future-of-crud-operations-in-wasp"},"Future of CRUD operations in Wasp"),(0,i.kt)("p",null,"CRUD operations currently have a limited set of knowledge about the business logic they are implementing."),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"For example, they don't know that a task should be connected to the user that is creating it. This is why we had to override the ",(0,i.kt)("inlineCode",{parentName:"li"},"create")," operation in the example above."),(0,i.kt)("li",{parentName:"ul"},"Another thing: they are not aware of the authorization rules. For example, they don't know that a user should not be able to create a task for another user. In the future, we will be adding role-based authorization to Wasp, and we plan to make CRUD operations aware of the authorization rules."),(0,i.kt)("li",{parentName:"ul"},"Another issue is input validation and sanitization. For example, we might want to make sure that the task description is not empty.")),(0,i.kt)("p",null,"CRUD operations are a mechanism for getting a backend up and running quickly, but it depends on the information it can get from the Wasp app. The more information that it can pick up from your app, the more powerful it will be out of the box. "),(0,i.kt)("p",null,"We plan on supporting CRUD operations and growing them to become the easiest way to create your backend. Follow along on ",(0,i.kt)("a",{parentName:"p",href:"https://github.com/wasp-lang/wasp/issues/1253"},"this Github issue")," to see how we are doing."),(0,i.kt)("hr",null),(0,i.kt)("p",null,"Join our ",(0,i.kt)("strong",{parentName:"p"},"community")," on ",(0,i.kt)("a",{parentName:"p",href:"https://discord.com/invite/rzdnErX"},"Discord"),", where we chat about full-stack web stuff. Join us to see what we are up to, share your opinions or get help with CRUD operations."))}m.isMDXComponent=!0}}]);