/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.mock.client;

import java.io.IOException;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.transform.HostTransformException;
import com.legstar.messaging.ContainerPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;
import com.legstar.test.coxb.lsfileac.QueryData;
import com.legstar.test.coxb.lsfileac.bind.QueryDataTransformers;
import com.legstar.test.coxb.lsfileac.bind.ReplyDataTransformers;
import com.legstar.test.coxb.lsfileac.bind.ReplyStatusTransformers;
import com.legstar.test.coxb.lsfileac.QueryLimit;
import com.legstar.test.coxb.lsfileac.bind.QueryLimitTransformers;
import com.legstar.test.coxb.lsfileac.ReplyItem;
import com.legstar.test.coxb.lsfileac.ReplyPersonal;
import com.legstar.test.coxb.lsfileac.ReplyStatus;
import com.legstar.test.coxb.lsfileac.ReplyData;

/**
 * Mocks the behavior of the LSFILEAE program.
 *
 */
public final class MockLsfileac {

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(MockLsfileac.class);
    
    /** Utility class.*/
    private MockLsfileac() {
        
    }

    /**
     * Create a response to LSFILEAE execution request.
     * @param requestMessage the request message
     * @return formatted response
     * @throws RequestException if response cannot be built
     */
    public static  LegStarMessage getResponse(
            final LegStarMessage requestMessage) throws RequestException {
        if (LOG.isDebugEnabled()) {
            LOG.debug("Building response for program LSFILEAC");
        }
        try {
            QueryDataTransformers queryDataTransformers = new QueryDataTransformers();
            QueryLimitTransformers queryLimitTransformers = new QueryLimitTransformers();
            QueryData queryData = null;
            QueryLimit queryLimit = null;
            for (LegStarMessagePart part : requestMessage.getDataParts()) {
                if (part.getPartID().equals("QueryData")) {
                    queryData = queryDataTransformers.toJava(part.getContent());
                }
                if (part.getPartID().equals("QueryLimit")) {
                    queryLimit = queryLimitTransformers.toJava(part.getContent());
                }
            }
            MockFILEA mockFILEA = new MockFILEA();
            String namePattern = (queryData == null) ? "*" : queryData.getQueryName();
            long maxItems = (queryLimit == null)
                    ?  mockFILEA.getCustomersList().size()
                    : queryLimit.getMaxItemsRead();
            List < Dfhcommarea > customers = mockFILEA.getCustomers(namePattern, maxItems);
            
            ReplyStatus replyStatus = new ReplyStatus();
            replyStatus.setTotalItemsRead(mockFILEA.getCustomersList().size());
            replyStatus.setSearchDuration("00:00:00");
            replyStatus.setReplyType(0);
            if (customers.size() > 0) {
                replyStatus.setReplyMessage("");
            } else {
                replyStatus.setReplyMessage("NO CUSTOMER SATISFIES YOUR QUERY");
            }
            replyStatus.setReplyResp(0);
            replyStatus.setReplyResp2(0);
            
            ReplyData replyData = null;
            if (customers.size() > 0) {
                replyData = new ReplyData();
                replyData.setReplyItemscount(customers.size());
                for (Dfhcommarea dfhcommarea : customers) {
                    ReplyItem replyItem = new ReplyItem();
                    replyItem.setReplyNumber(dfhcommarea.getComNumber());
                    ReplyPersonal replyPersonal = new ReplyPersonal();
                    replyPersonal.setReplyName(dfhcommarea.getComPersonal().getComName());
                    replyPersonal.setReplyAddress(dfhcommarea.getComPersonal().getComAddress());
                    replyPersonal.setReplyPhone(dfhcommarea.getComPersonal().getComPhone());
                    replyItem.setReplyPersonal(replyPersonal);
                    replyItem.setReplyDate(dfhcommarea.getComDate());
                    replyItem.setReplyAmount(dfhcommarea.getComAmount());
                    replyItem.setReplyComment(dfhcommarea.getComComment());
                    replyData.getReplyItem().add(replyItem);
                }
            }
            
            ReplyStatusTransformers replyStatusTransformers = new ReplyStatusTransformers();
            LegStarMessage replyMessage = new LegStarMessage();
            replyMessage.addDataPart(new ContainerPart("ReplyStatus",
                    replyStatusTransformers.toHost(replyStatus)));
            ReplyDataTransformers replyDataTransformers = new ReplyDataTransformers();
            replyMessage.addDataPart(new ContainerPart("ReplyData",
                    (replyData == null) ? null : replyDataTransformers.toHost(replyData)));
            return replyMessage;
        } catch (HeaderPartException e) {
            throw new RequestException(e);
        } catch (HostTransformException e) {
            throw new RequestException(e);
        } catch (IOException e) {
            throw new RequestException(e);
        }
    }
    
}
