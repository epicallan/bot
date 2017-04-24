// we need to create callbackUrl

// _setupNewWebhook() {
//   const oAuthUrl = 'https://graph.facebook.com/v2.7/oauth/access_token' +
//     '?client_id=' + this.config.applicationID +
//     '&client_secret=' + this.config.appSecret +
//     '&grant_type=client_credentials'
//
//   const url = `https://graph.facebook.com/v2.7/${this.config.applicationID}/subscriptions?access_token=`
//
//   return fetch(oAuthUrl)
//   .then(this._handleFacebookResponse)
//   .then(res => res.json())
//   .then(json => json.access_token)
//   .then(token => fetch(url + token, {
//     method: 'POST',
//     headers: {'Content-Type': 'application/json'},
//     body: JSON.stringify({
//       object: 'page',
//       callback_url: 'https://' + this.config.hostname + '/api/botpress-messenger/webhook',
//       verify_token: this.config.verifyToken,
//       fields: [
//         'message_deliveries',
//         'message_reads',
//         'messages',
//         'messaging_optins',
//         'messaging_postbacks',
//         'messaging_referrals'
//       ]
//     })
//   }))
//   .then(this._handleFacebookResponse)
//   .then(res => res.json())
// }
