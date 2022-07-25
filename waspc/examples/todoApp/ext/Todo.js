import React, { useState } from 'react'
import { Link } from 'react-router-dom'

import Button from '@mui/material/Button'
import TextField from '@mui/material//TextField'
import Paper from '@mui/material//Paper'
import Table from '@mui/material//Table'
import TableBody from '@mui/material//TableBody'
import TableCell from '@mui/material//TableCell'
import TableRow from '@mui/material//TableRow'
import Checkbox from '@mui/material//Checkbox'

import { useQuery } from '@wasp/queries'
import getTasks from '@wasp/queries/getTasks.js'
import createTask from '@wasp/actions/createTask.js'
import updateTaskIsDone from '@wasp/actions/updateTaskIsDone.js'
import deleteCompletedTasks from '@wasp/actions/deleteCompletedTasks.js'
import toggleAllTasks from '@wasp/actions/toggleAllTasks.js'

const Todo = (props) => {
  const { data: tasks, isError, error: tasksError } = useQuery(getTasks)

  const isThereAnyTask = () => tasks?.length > 0

  const TasksError = (props) => {
    return 'Error during fetching tasks: ' + (tasksError?.message || '')
  }

  return (
    <div className='todos'>
      <div className='todos__container'>
        <h1> Todos </h1>

        <div className='todos__toggleAndInput'>
          <ToggleAllTasksButton disabled={!isThereAnyTask()} />
          <NewTaskForm />
        </div>

        {isError && <TasksError />}

        {isThereAnyTask() && (
          <>
            <Tasks tasks={tasks} />

            <Footer tasks={tasks} />
          </>
        )}
      </div>
    </div>
  )
}

const Footer = (props) => {
  const numCompletedTasks = props.tasks.filter(t => t.isDone).length
  const numUncompletedTasks = props.tasks.filter(t => !t.isDone).length

  const handleDeleteCompletedTasks = async () => {
    try {
      await deleteCompletedTasks()
    } catch (err) {
      console.log(err)
      window.alert('Error:' + err.message)
    }
  }

  return (
    <div className='todos__footer'>
      <div className='todos__footer__itemsLeft'>
        {numUncompletedTasks} items left
      </div>

      <div className='todos__footer__clearCompleted'>
        <Button
          className={numCompletedTasks > 0 ? '' : 'hidden'}
          variant="contained" color="secondary"
          onClick={handleDeleteCompletedTasks}
        >
          Delete completed
        </Button>
      </div>
    </div>
  )
}

const Tasks = (props) => {
  return (
    <div>
      <Paper>
        <Table size="small">
          <TableBody>
            {props.tasks.map((task, idx) => <Task task={task} key={idx} />)}
          </TableBody>
        </Table>
      </Paper>

    </div>
  )
}

const Task = (props) => {
  const handleTaskIsDoneChange = async (event) => {
    const id = parseInt(event.target.id)
    const isDone = event.target.checked

    try {
      await updateTaskIsDone({ id, isDone })
    } catch (err) {
      console.log(err)
      window.alert('Error:' + err.message)
    }
  }

  return (
    <TableRow>
      <TableCell>
        <Checkbox
          id={String(props.task.id)}
          checked={props.task.isDone}
          onChange={handleTaskIsDoneChange}
          color="default"
        />
      </TableCell>
      <TableCell>
        <Link to={`/task/${props.task.id}`}> {props.task.description} </Link>
      </TableCell>
    </TableRow>
  )
}

const NewTaskForm = (props) => {
  const defaultDescription = ''
  const [description, setDescription] = useState(defaultDescription)

  const createNewTask = async (description) => {
    const task = { isDone: false, description }
    await createTask(task)
  }

  const handleNewTaskSubmit = async (event) => {
    event.preventDefault()
    try {
      await createNewTask(description)
      setDescription(defaultDescription)
    } catch (err) {
      console.log(err)
      window.alert('Error:' + err.message)
    }
  }

  return (
    <form onSubmit={handleNewTaskSubmit}>
      <TextField
        className="todos__newTaskForm__input"
        placeholder="Enter task"
        value={description}
        onChange={e => setDescription(e.target.value)}
      />
      <Button
        variant="contained" color="primary" type='submit'
      >
        Create new task
      </Button>
    </form>
  )
}

const ToggleAllTasksButton = (props) => {
  const handleToggleAllTasks = async () => {
    try {
      await toggleAllTasks()
    } catch (err) {
      console.log(err)
      window.alert('Error:' + err.message)
    }
  }

  return (
    <Button
      variant="contained" color="primary"
      disabled={props.disabled}
      onClick={handleToggleAllTasks}
    >
      ✓
    </Button>
  )
}

export default Todo
