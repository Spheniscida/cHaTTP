# ifndef PARSE_PERSISTENT_MESSAGE_HPP
# define PARSE_PERSISTENT_MESSAGE_HPP

# include <string>
# include <vector>
# include <sstream>

# include <persistence.pb.h>

# include "sequence-number.hpp"
# include "receivable.hpp"

using std::string;
using std::vector;
using std::istringstream;

using chattp::PersistenceResponse;
using chattp::PersistenceRequest;

/*********************** Parsing and representing protocol responses ***************************/

/**
 * @brief A generic response from the persistence layer
 *
 * Most fields are only filled for specific response types:
 *
 * - sequence_number exists in any instance.
 * - status too.
 * - online, broker_name, channel_name: only for ULKDUP
 * - messages only for MSGS
 *
 */
class PersistenceLayerResponse : public Receivable
{
public:
    PersistenceLayerResponse(const char* buffer, size_t length);

    // for direct operations on the protobuf object, e.g. collecting repeated elements
    const chattp::PersistenceResponse& get_protobuf(void) const { return response_buffer; }

    // Some shortcuts so the protobuf doesn't leak too much.
    const sequence_t sequence_number(void) const { return response_buffer.sequence_number(); }
    const bool status(void) const { return response_buffer.status(); }
    const PersistenceResponse::PersistenceResponseType type(void) const { return response_buffer.type(); }

private:
    chattp::PersistenceResponse response_buffer;

};

/************************************* Creating protocol messages **************************************/

class PersistenceLayerCommand
{
public:
    /// For MSGGT, LOGOUT, ULKUP
    PersistenceLayerCommand(PersistenceRequest::PersistenceRequestType code, const string& user_name);
    /// For ULKUP
    PersistenceLayerCommand(PersistenceRequest::PersistenceRequestType code, const vector<string>& user_name);
    /// For UREG, CHKPASS
    PersistenceLayerCommand(PersistenceRequest::PersistenceRequestType code, const string& user, const string& broker, const string& channel);
    /// For LOGIN
    PersistenceLayerCommand(PersistenceRequest::PersistenceRequestType code, const string& user, const string& password);
    /// For MSGSV
    PersistenceLayerCommand(PersistenceRequest::PersistenceRequestType code, const chattp::ChattpMessage& message);

    const chattp::PersistenceRequest& get_protobuf(void) const { return request_buffer; }
    string toString(void) const;
    sequence_t sequence_number(void) const { return request_buffer.sequence_number(); };
private:

    chattp::PersistenceRequest request_buffer;
};

# endif
