# ZCurl Installation & Setup Guide

## Prerequisites

Before installing ZCurl, ensure you have the following:

### Required Dependencies
1. **abap2UI5 Framework**: Install the latest version from [abap2UI5 GitHub](https://github.com/abap2UI5/abap2UI5)
2. **abap-odata-test Library**: The REST client libraries from `~/Downloads/src/abap-odata-test`
3. **SAP System**: ABAP 7.02+ or ABAP Cloud environment

### System Requirements
- SAP NetWeaver 7.02 or higher / S/4HANA / BTP ABAP Environment
- HTTP/HTTPS connectivity for external API calls
- Modern web browser (Chrome, Firefox, Safari, Edge)

## Installation Steps

### Step 1: Install Dependencies

1. **Install abap2UI5 Framework**
   ```bash
   # Clone and install abap2UI5 using abapGit
   # Repository: https://github.com/abap2UI5/abap2UI5
   ```

2. **Install REST Client Libraries**
   - Ensure the abap-odata-test package is installed in your system
   - This provides the base REST client functionality

### Step 2: Install ZCurl

1. **Using abapGit** (Recommended)
   ```bash
   # Clone this repository using abapGit
   # Repository URL: [Your Git Repository URL]
   ```

2. **Manual Installation**
   - Copy all files from `src/` folder to your SAP system
   - Create package `ZCURL` in SE80/ADT
   - Import all classes and programs

### Step 3: Configure HTTP Service

1. **For On-Premise Systems (SICF)**
   ```abap
   " Create new service in SICF (Transaction SICF)
   " Service Path: /sap/bc/rest/zcurl
   " Handler Class: Z2UI5_CL_HTTP_HANDLER (from abap2UI5)
   ```

2. **For Cloud Systems**
   ```abap
   " Create HTTP service in ADT
   " Service ID: ZCURL_HTTP_SERVICE
   " Handler Class: Z2UI5_CL_HTTP_HANDLER
   ```

### Step 4: Security Configuration

1. **SSL Certificates** (For HTTPS calls)
   - Import target API certificates via STRUST
   - Configure SSL client for external connections

2. **Authorization Objects**
   - Assign appropriate authorizations for HTTP client usage
   - Configure user roles for ZCurl access

## Configuration

### Environment Variables

ZCurl supports environment variables for different deployment stages:

```json
{
  "development": {
    "base_url": "https://api-dev.example.com",
    "api_key": "dev_key_123",
    "timeout": 30
  },
  "production": {
    "base_url": "https://api.example.com", 
    "api_key": "prod_key_456",
    "timeout": 60
  }
}
```

### Default Headers

Configure common headers for your organization:

```abap
" Add to initialization method
APPEND VALUE #( key = 'User-Agent' value = 'ZCurl-SAP-Client/1.0' enabled = abap_true ) TO mt_headers.
APPEND VALUE #( key = 'Accept' value = 'application/json' enabled = abap_true ) TO mt_headers.
```

## Usage Examples

### Basic GET Request

1. Launch ZCurl application
2. Select `GET` method
3. Enter URL: `https://jsonplaceholder.typicode.com/users`
4. Click `Send`
5. View response in Response tab

### POST Request with Authentication

1. Select `POST` method
2. Enter URL: `https://api.example.com/users`
3. Go to `Authorization` tab
4. Select `Bearer Token`
5. Enter your API token
6. Go to `Body` tab
7. Enter JSON payload:
   ```json
   {
     "name": "John Doe",
     "email": "john@example.com"
   }
   ```
8. Click `Send`

### Using Environment Variables

1. Configure environment in sidebar
2. Use variables in URL: `{{base_url}}/users`
3. Variables are automatically replaced when sending

## Advanced Features

### Collections Management

- Save frequently used requests
- Organize requests by functionality
- Share collections between team members

### Request History

- Automatic saving of request/response pairs
- Search through previous requests
- Export request history

### Custom Authentication

```abap
" Extend zcl_curl_http_service for custom auth
METHOD add_authentication.
  CASE iv_config-auth_type.
    WHEN 'oauth2'.
      " Implement OAuth2 flow
      DATA(token) = get_oauth2_token( ).
      APPEND VALUE #( name = 'Authorization' value = |Bearer { token }| ) TO ct_headers.
  ENDCASE.
ENDMETHOD.
```

## Troubleshooting

### Common Issues

1. **"Cannot connect to host"**
   - Check network connectivity
   - Verify SSL certificates in STRUST
   - Check proxy settings

2. **"Authorization failed"**
   - Verify API credentials
   - Check authentication method
   - Confirm API endpoint permissions

3. **"Request timeout"**
   - Increase timeout value
   - Check target server response time
   - Verify network stability

### Debug Mode

Enable debug mode for detailed logging:

```abap
" In zcl_curl_app_enhanced->handle_send_request
DATA(debug_mode) = abap_true.
IF debug_mode = abap_true.
  " Log request details
  cl_demo_output=>write_data( ls_config ).
ENDIF.
```

### Log Analysis

Check system logs for HTTP-related issues:
- Transaction SLG1 (Application Log)
- SM21 (System Log)
- ST22 (ABAP Dumps)

## Extending ZCurl

### Adding New Authentication Types

1. Extend `ty_auth_config` structure
2. Add new case in `add_authentication` method
3. Update UI in `build_request_panel`

### Custom Response Formatters

```abap
" Add to zcl_curl_http_service=>format_response_body
WHEN 'application/xml'.
  " Custom XML formatting
  result = format_xml_pretty( iv_body ).
```

### Plugin Architecture

Create custom plugins for specific API types:

```abap
CLASS zcl_curl_plugin_oauth2 DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_curl_plugin.
ENDCLASS.
```

## Integration Examples

### SAP Cloud Platform Integration

```abap
" Configure for CPI endpoints
ls_config-url = 'https://your-tenant.integrationsuite.cfapps.us10.hana.ondemand.com/http/your-endpoint'.
ls_config-auth_type = 'oauth2'.
```

### S/4HANA Cloud API Integration

```abap
" Configure for S/4HANA Cloud APIs
ls_config-url = 'https://my123456.s4hana.ondemand.com/sap/opu/odata/'.
ls_config-auth_type = 'basic'.
```

## Performance Optimization

### Caching Strategies

```abap
" Implement response caching
METHOD cache_response.
  " Cache based on URL + headers hash
  DATA(cache_key) = get_cache_key( iv_url = url it_headers = headers ).
  " Store/retrieve from shared memory
ENDMETHOD.
```

### Connection Pooling

```abap
" Reuse HTTP connections where possible
METHOD get_http_client.
  " Check for existing connections
  " Return pooled connection if available
ENDMETHOD.
```

## Support & Contributing

### Getting Help

1. Check documentation and examples
2. Search existing issues on GitHub
3. Create new issue with detailed description
4. Join community discussions

### Contributing

1. Fork the repository
2. Create feature branch
3. Implement changes with tests
4. Submit pull request
5. Follow code review process

### Code Standards

- Follow ABAP naming conventions
- Add comprehensive documentation
- Include unit tests
- Use consistent formatting

## License

MIT License - see LICENSE file for details.

## Changelog

### Version 1.0.0
- Initial release
- Basic HTTP methods support
- Request/Response management
- Collection management
- Environment variables
- Authentication support

### Future Roadmap
- WebSocket support
- GraphQL support
- Test automation
- Team collaboration features
- API documentation generation
