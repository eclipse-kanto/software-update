# Eclipse hawkBit™ - Client library for Golang

This repository contains the Golang implementation of [hawkBit SoftwareUpdatable](https://github.com/eclipse/vorto/tree/development/models/org.eclipse.hawkbit.swupdatable-SoftwareUpdatable-2.0.0.fbmodel) feature.

The [Eclipse Ditto Client for Golang](https://github.com/eclipse/ditto-clients-golang) is used for communication.

Table of Contents
-----------------
* [Installation](#Installation)
* [Creating and activating a software updatable feature](#Creating-and-activating-a-software-updatable-feature)
* [Working with software updatable feature](#Working-with-software-updatable-feature)
    * [Updating installed and context dependencies](#Updating-installed-and-context-dependencies)
    * [Handle install operation](#Handle-install-operation)
* [Logging](#Logging)

## Installation

```
go get github.com/eclipse/hawkbit-clients-golang
```

## Creating and activating a software updatable feature

Each software updatable feature instance requires a ditto.Client and hawkbit.Configuration.

```go
var su *hawkbit.SoftwareUpdatable

// Activate the SoftwareUpdatable feature after Ditto Client is connected.
configDitto := ditto.NewConfiguration().
	WithBroker("mqtt-host:1883").
	WithConnectHandler(func(dittoClient *ditto.Client) {
		if err := su.Activate(); err != nil {
			panic(fmt.Errorf("cannot activate software updatable feature: %v", err))
		}
	})

// Create new Ditto Client instance.
dittoClient := ditto.NewClient(configDitto)

// Create hawkBit SoftwareUpdatable feature configuration.
config := hawkbit.NewConfiguration().
	WithDittoClient(dittoClient).
	WithThingID(model.NewNamespacedIDFrom("my.namespace:thing.id")).
	WithSoftwareType("my-type").
	WithInstallHandler(installHandler)

// Create new hawkBit SoftwareUpdatable instance.
su, err = hawkbit.NewSoftwareUpdatable(config)
if err != nil {
	panic(fmt.Errorf("failed to create software updatable: %v", err))
}
```

**_NOTE:_** All feature propertires can be modified before the feature activation. This will change the initial feature property values.

After you have configured and created software updatable instance, the Ditto client is ready to be connected.

```go
if err := dittoClient.Connect(); err != nil {
    panic(fmt.Errorf("cannot connect to broker: %v", err))
}
```

It's a good practice to deactivate the feature on client disconnect.

```go
su.Deactivate()
dittoClient.Disconnect()
```

## Working with software updatable feature

### Update installed and context dependencies

```go
// Create new DependencyDescription.
dependency := &hawkbit.DependencyDescription{Group: "My Group", Name: "App #1", Version: "1.0.0", Type: "my-type"}

// Update installed dependencies property.
if err := su.SetInstalledDependencies(dependency); err != nil {
	fmt.Println(fmt.Errorf("could not update installed dependencies property: %v", err))
}

// Update context dependencies property.
if err := su.SetContextDependencies(dependency); err != nil {
	fmt.Println(fmt.Errorf("could not update context dependencies property: %v", err))
}
```

### Handle install operation

```go
func installHandler(update *hawkbit.SoftwareUpdateAction, su *hawkbit.SoftwareUpdatable) {
    // Install provided software modules.
	startProgress := 0
	for _, module := range update.SoftwareModules {
		status := hawkbit.NewOperationStatusUpdate(update.CorrelationID, hawkbit.StatusStarted, module.SoftwareModule).
			WithProgress(&startProgress).WithMessage("install operation just started")
		if err := su.SetLastOperation(status); err != nil {
			fmt.Println(fmt.Errorf("could not update the last operation: %v", err))
		}
		// Do the installation here.
	}
    // Finally update the installed dependencies.
}
```
**_NOTE:_** The last failed operation will be updated automatically, if needed.

## Logging

Various levels of logs are provided by assigning the logging endpoints, ERROR, WARN, INFO and DEBUG. For example:

```go
type LogLevel int

const (
	ERROR LogLevel = 1 + iota
	WARN
	INFO
	DEBUG
)

var level = INFO

func init() {
	hawkbit.ERROR = &wrapper{level: ERROR, prefix: "ERROR  "}
	hawkbit.WARN = &wrapper{level: WARN, prefix: "WARN   "}
	hawkbit.INFO = &wrapper{level: INFO, prefix: "INFO   "}
	hawkbit.DEBUG = &wrapper{level: DEBUG, prefix: "DEBUG  "}
}

type wrapper struct {
	level  LogLevel
	prefix string
}

func (w *wrapper) Println(v ...interface{}) {
	if level >= w.level {
		fmt.Println(w.prefix, fmt.Sprint(v...))
	}
}

func (w *wrapper) Printf(format string, v ...interface{}) {
	if level >= w.level {
		fmt.Printf(fmt.Sprint(w.prefix, " ", format), v...)
	}
}
```
