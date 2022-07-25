{{={= =}=}}
import React from 'react'
import { createRoot } from 'react-dom/client'
import { QueryClientProvider } from '@tanstack/react-query'

import router from './router'
import { 
  initializeQueryClient,
  queryClientInitialized,
} from './queryClient'
import * as serviceWorker from './serviceWorker'

{=# doesClientSetupFnExist =}
{=& clientSetupJsFnImportStatement =}
{=/ doesClientSetupFnExist =}

import './index.css'

startApp()

async function startApp() {
  {=# doesClientSetupFnExist =}
  await {= clientSetupJsFnIdentifier =}()
  {=/ doesClientSetupFnExist =}
  initializeQueryClient()

  await render()

  // If you want your app to work offline and load faster, you can change
  // unregister() to register() below. Note this comes with some pitfalls.
  // Learn more about service workers: https://bit.ly/CRA-PWA
  serviceWorker.unregister()
}

async function render() {
  const queryClient = await queryClientInitialized
  const container = document.getElementById('root')
  const root = createRoot(container)
  root.render(
    <QueryClientProvider client={queryClient}>
      { router }
    </QueryClientProvider>
  )
}
