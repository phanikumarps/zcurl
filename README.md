# ZCurl - Postman-like UI for ABAP

A Postman-like REST client built with abap2UI5 for testing REST APIs and OData services directly from SAP systems.

## Features

- Modern UI5/Fiori-like interface built purely in ABAP
- Support for all HTTP methods (GET, POST, PUT, DELETE, PATCH)
- Request/Response history management
- Headers and authentication management
- Request body formatting (JSON, XML, form-data)
- Response visualization and formatting
- Collection management for grouping related requests
- Environment variables support
- Integration with existing REST client libraries

## Installation

1. Install this repository using abapGit
2. Ensure abap2UI5 framework is installed in your system
3. Configure HTTP service endpoint for the application

## Usage

1. Navigate to the ZCurl application in your SAP system
2. Enter the API endpoint URL
3. Select HTTP method
4. Add headers and authentication if needed
5. Configure request body (for POST/PUT operations)
6. Send request and view response
7. Save request to collection for reuse

## Technical Architecture

The application follows abap2UI5 patterns:
- Pure ABAP development with UI5 frontend
- Leverages existing REST client libraries from abap-odata-test
- Modular design with pluggable backend services
- Clean separation of concerns

## Dependencies

- abap2UI5 framework
- abap-odata-test REST client libraries (for backend HTTP calls)

## License

MIT License
